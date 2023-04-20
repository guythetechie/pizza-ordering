[<RequireQualifiedAccess>]
module api.Order

open Asp.Versioning.Builder
open common
open FSharpPlus
open FSharpPlus.Data
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Routing
open Microsoft.Extensions.DependencyInjection
open System
open System.Threading.Tasks
open System.Net
open System.Threading
open System.Text.Json.Nodes

[<RequireQualifiedAccess>]
module Create =
    let private toValidation (result: Result<_, string>) =
        result |> Result.mapError List.singleton |> Validation.ofResult

    let private tryGetId (request: HttpRequest) =
        request.RouteValues["id"]
        |> string
        |> Guid.TryParse
        |> function
            | true, guid -> OrderId guid |> Ok
            | false, _ ->
                Error
                    { ApiErrorWithStatusCode.Error =
                        { ApiError.Code = ApiErrorCode.InvalidRouteValue
                          Message = NonEmptyString.fromString "Order ID must be a valid GUID."
                          Details = List.empty }

                      StatusCode = HttpStatusCode.BadRequest }

    let private tryGetConditionalHeaderAction (request: HttpRequest) =
        match (request.Headers.TryGetValue("If-Match"), request.Headers.TryGetValue("If-None-Match")) with
        | (true, _), (true, _) ->
            Error
                { ApiErrorWithStatusCode.Error =
                    { ApiError.Code = ApiErrorCode.InvalidConditionalHeader
                      Message = NonEmptyString.fromString "Cannot specify both 'If-Match' and 'If-None-Match' headers."
                      Details = List.empty }

                  StatusCode = HttpStatusCode.BadRequest }

        | (true, eTag), (false, _) -> NonEmptyString.fromString eTag |> ETag |> ConditionalHeaderAction.Update |> Ok
        | (false, _), (true, ifMatchHeader) when String.Equals(ifMatchHeader, "*") -> Ok ConditionalHeaderAction.Create
        | (false, _), (true, _) ->
            Error
                { ApiErrorWithStatusCode.Error =
                    { ApiError.Code = ApiErrorCode.InvalidConditionalHeader
                      Message = NonEmptyString.fromString "'If-None-Match' header must be '*'."
                      Details = List.empty }

                  StatusCode = HttpStatusCode.BadRequest }
        | (false, _), (false, _) ->
            Error
                { ApiErrorWithStatusCode.Error =
                    { ApiError.Code = ApiErrorCode.InvalidConditionalHeader
                      Message = NonEmptyString.fromString "'If-Match' or 'If-None-Match' header must be specified."
                      Details = List.empty }

                  StatusCode = HttpStatusCode.PreconditionRequired }

    let private tryGetRequestBodyAsJsonObject (request: HttpRequest) =
        JsonObject.tryFromStream request.Body
        |> AsyncResult.setError
            { ApiErrorWithStatusCode.Error =
                { ApiError.Code = ApiErrorCode.InvalidJsonBody
                  Message = NonEmptyString.fromString "Request body is not a JSON object."
                  Details = List.empty }

              StatusCode = HttpStatusCode.BadRequest }

    let private validatePickupTime jsonObject =
        JsonObject.tryGetDateTimeOffsetProperty "pickupTime" jsonObject
        |> Result.map PickupTime
        |> toValidation

    let private validatePizzaTopping jsonObject =
        validationCE {
            let! kind =
                JsonObject.tryGetStringProperty "topping" jsonObject
                |> Result.bind ToppingKind.tryFromString
                |> toValidation

            and! amount =
                JsonObject.tryGetStringProperty "amount" jsonObject
                |> Result.bind ToppingAmount.tryFromString
                |> toValidation

            return { Topping.Kind = kind; Amount = amount }
        }

    let private validatePizzaToppings jsonObject =
        JsonObject.tryGetJsonObjectArrayProperty "toppings" jsonObject
        |> toValidation
        |> Validation.bind (Validation.traverseBiApplyList validatePizzaTopping)

    let private validatePizza jsonObject =
        validationCE {
            let! size =
                JsonObject.tryGetStringProperty "size" jsonObject
                |> Result.bind PizzaSize.tryFromString
                |> toValidation

            and! toppings = validatePizzaToppings jsonObject

            return
                { Pizza.Size = size
                  Toppings = toppings }
        }

    let private validatePizzas jsonObject =
        JsonObject.tryGetJsonObjectArrayProperty "pizzas" jsonObject
        |> toValidation
        |> Validation.bind (Validation.traverseBiApplyList validatePizza)

    let private tryGetRequestBodyFromJsonObject jsonObject =
        validationCE {
            let! pickupTime = validatePickupTime jsonObject
            and! pizzas = validatePizzas jsonObject

            return (pickupTime, pizzas)
        }
        |> Validation.toResult
        |> Result.mapError (fun errors ->
            { ApiErrorWithStatusCode.Error =
                { ApiError.Code = ApiErrorCode.InvalidJsonBody
                  Message = NonEmptyString.fromString "Request body is invalid."
                  Details =
                    errors
                    |> List.map (fun error ->
                        { ApiError.Code = ApiErrorCode.InvalidJsonBody
                          Message = NonEmptyString.fromString error
                          Details = List.empty }) }

              StatusCode = HttpStatusCode.BadRequest })

    let private tryGetRequestBody request =
        tryGetRequestBodyAsJsonObject request
        |> Async.map (Result.bind tryGetRequestBodyFromJsonObject)

    let private tryGetRequestParameters request =
        asyncResultCE {
            let! (pickupTime, pizzas) = tryGetRequestBody request
            and! id = tryGetId request
            and! headerAction = tryGetConditionalHeaderAction request

            let order =
                { Order.Id = id
                  Pizzas = pizzas
                  PickupTime = pickupTime }

            return (headerAction, order)
        }

    let private serializeOrder (order: Order) =
        let serializePickupTime pickupTime =
            PickupTime.toDateTimeOffset pickupTime |> JsonValue.Create

        let serializeSize size =
            PizzaSize.toString size |> JsonValue.Create

        let serializeToppingKind toppingKind =
            ToppingKind.toString toppingKind |> JsonValue.Create

        let serializeToppingAmount toppingAmount =
            ToppingAmount.toString toppingAmount |> JsonValue.Create

        let serializeTopping (topping: Topping) =
            let jsonObject = new JsonObject()
            jsonObject.Add("topping", serializeToppingKind topping.Kind)
            jsonObject.Add("amount", serializeToppingAmount topping.Amount)
            jsonObject

        let serializeToppings toppings =
            List.map serializeTopping toppings |> JsonArray.fromNodes

        let serializePizza (pizza: Pizza) =
            let jsonObject = new JsonObject()
            jsonObject.Add("size", serializeSize pizza.Size)
            jsonObject.Add("toppings", serializeToppings pizza.Toppings)
            jsonObject

        let serializePizzas pizzas =
            List.map serializePizza pizzas |> JsonArray.fromNodes

        let jsonObject = new JsonObject()
        jsonObject.Add("pickupTime", serializePickupTime order.PickupTime)
        jsonObject.Add("pizzas", serializePizzas order.Pizzas)
        jsonObject

    let private processCreateRequest (requestUri: Uri) createOrder order =
        async {
            match! createOrder order with
            | Ok eTag ->
                let orderJson = serializeOrder order
                orderJson.Add("eTag", ETag.toString eTag)
                return TypedResults.Created(requestUri, orderJson) :> IResult
            | Error ApiErrorCode.ResourceAlreadyExists ->
                return
                    TypedResults.Conflict(
                        { ApiError.Code = ApiErrorCode.ResourceAlreadyExists
                          Message =
                            NonEmptyString.fromString $"An order with ID {OrderId.toString order.Id} already exists."
                          Details = List.empty }
                    )
            | _ -> return (NotImplementedException() |> raise)
        }

    let private processUpdateRequest updateOrder eTag order =
        async {
            match! updateOrder eTag order with
            | Ok eTag ->
                let orderJson = serializeOrder order
                orderJson.Add("eTag", ETag.toString eTag)
                return TypedResults.Ok(orderJson) :> IResult
            | Error ApiErrorCode.ResourceNotFound ->
                return
                    TypedResults.NotFound(
                        { ApiError.Code = ApiErrorCode.ResourceNotFound
                          Message =
                            NonEmptyString.fromString $"An order with ID {OrderId.toString order.Id} was not found."
                          Details = List.empty }
                    )
            | Error ApiErrorCode.ETagMismatch ->
                return
                    TypedResults.Json(
                        { ApiError.Code = ApiErrorCode.ETagMismatch
                          Message =
                            NonEmptyString.fromString
                                $"The If-Match header eTag doesn't match the resource's. Another process might have updated it. Please get a new ETag and try again."
                          Details = List.empty },
                        statusCode = (int) HttpStatusCode.PreconditionFailed
                    )
            | _ -> return (NotImplementedException() |> raise)
        }

    let private processRequest createOrder updateOrder requestUri headerAction order =
        match headerAction with
        | ConditionalHeaderAction.Create -> processCreateRequest requestUri createOrder order
        | ConditionalHeaderAction.Update eTag -> processUpdateRequest updateOrder eTag order

    let private handle createOrder updateOrder request cancellationToken =
        async {
            match! tryGetRequestParameters request with
            | Ok(headerAction, order) ->
                let requestUri =
                    new Uri($"{request.Scheme}://{request.Host}{request.PathBase}{request.Path}", UriKind.Absolute)

                return! processRequest createOrder updateOrder requestUri headerAction order
            | Error error -> return ApiErrorWithStatusCode.toIResult error
        }
        |> Async.startAsTaskWithCancellation cancellationToken

    type CreateOrder = Order -> Async<Result<ETag, ApiErrorCode>>

    type UpdateOrder = ETag -> Order -> Async<Result<ETag, ApiErrorCode>>

    let private handler =
        Func<CreateOrder, UpdateOrder, HttpRequest, CancellationToken, Task<IResult>>(handle)

    [<RequireQualifiedAccess>]
    module Services =
        let private createOrder (provider: IServiceProvider) : CreateOrder =
            fun order ->
                Guid.NewGuid().ToString()
                |> NonEmptyString.fromString
                |> ETag.fromString
                |> Ok
                |> async.Return

        let private updateOrder (provider: IServiceProvider) : UpdateOrder =
            fun eTag order -> Result.Ok eTag |> async.Return

        let configure (services: IServiceCollection) =
            services
                .AddSingleton<CreateOrder>(Func<IServiceProvider, CreateOrder>(createOrder))
                .AddSingleton<UpdateOrder>(Func<IServiceProvider, UpdateOrder>(updateOrder))
            |> ignore

    [<RequireQualifiedAccess>]
    module internal Endpoints =
        let configure (builder: IEndpointRouteBuilder) = builder.MapPut("/{id}", handler)

let configureEndpoints (builder: IVersionedEndpointRouteBuilder) =
    let groupBuilder =
        builder.MapGroup("/v{version:apiVersion}/orders").HasApiVersion(1)

    Create.Endpoints.configure groupBuilder
