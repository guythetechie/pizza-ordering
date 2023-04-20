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
module CreateOrReplace =
    let private toValidation (result: Result<_, string>) =
        result |> Result.mapError List.singleton |> Validation.ofResult

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

    let private validateBodyJson jsonObject =
        validationCE {
            let! pickupTime = validatePickupTime jsonObject
            and! pizzas = validatePizzas jsonObject

            return (pickupTime, pizzas)
        }
        |> Validation.toResult

    let private parametersToResource id body =
        let (pickupTime, pizzas) = body

        { Order.Id = id
          Pizzas = pizzas
          PickupTime = pickupTime }

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

    let private handle createOrder updateOrder request cancellationToken =
        Handlers.CreateOrReplace.handle
            createOrder
            updateOrder
            serializeOrder
            (fun order -> OrderId.toString order.Id)
            validateBodyJson
            parametersToResource
            request
            cancellationToken

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

    CreateOrReplace.Endpoints.configure groupBuilder
