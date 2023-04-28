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
open System.Threading
open System.Threading.Tasks
open System.Text.Json.Nodes
open System.Text.Json

[<RequireQualifiedAccess>]
module private Common =
    let tryGetId (idString: string) =
        Guid.TryParse idString
        |> function
            | true, guid -> OrderId guid |> Ok
            | false, _ ->
                TypedResults.BadRequest(
                    { ApiError.Code = ApiErrorCode.InvalidRouteValue
                      Message = NonEmptyString.fromString "ID must be a valid GUID."
                      Details = List.empty }
                )
                :> IResult
                |> Error

[<RequireQualifiedAccess>]
module Serialization =
    let serializeOrder (order: Order) =
        let serializeId orderId =
            OrderId.toString orderId |> JsonValue.Create

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
        jsonObject.Add("id", serializeId order.Id)
        jsonObject.Add("pickupTime", serializePickupTime order.PickupTime)
        jsonObject.Add("pizzas", serializePizzas order.Pizzas)
        jsonObject

    let tryDeserializeOrder jsonObject =
        let toValidation (result: Result<_, string>) =
            result |> Result.mapError List.singleton |> Validation.ofResult

        let validateId jsonObject =
            JsonObject.tryGetGuidProperty "id" jsonObject
            |> Result.map OrderId
            |> toValidation

        let validatePickupTime jsonObject =
            JsonObject.tryGetDateTimeOffsetProperty "pickupTime" jsonObject
            |> Result.map PickupTime
            |> toValidation

        let validatePizzaTopping jsonObject =
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

        let validatePizzaToppings jsonObject =
            JsonObject.tryGetJsonObjectArrayProperty "toppings" jsonObject
            |> toValidation
            |> Validation.bind (Validation.traverseBiApplyList validatePizzaTopping)

        let validatePizza jsonObject =
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

        let validatePizzas jsonObject =
            JsonObject.tryGetJsonObjectArrayProperty "pizzas" jsonObject
            |> toValidation
            |> Validation.bind (Validation.traverseBiApplyList validatePizza)

        validationCE {
            let! id = validateId jsonObject
            and! pickupTime = validatePickupTime jsonObject
            and! pizzas = validatePizzas jsonObject

            return
                { Order.Id = id
                  PickupTime = pickupTime
                  Pizzas = pizzas }
        }
        |> Validation.toResult

    let deserializeOrder jsonObject =
        tryDeserializeOrder jsonObject
        |> Result.defaultWith (fun errors -> JsonException(String.concat "; " errors) |> raise)


[<RequireQualifiedAccess>]
module CreateOrReplace =
    let private tryGetOrder (id: string) (jsonObject: JsonObject) =
        jsonObject.Add("id", id)

        Serialization.tryDeserializeOrder jsonObject
        |> Result.mapError (fun errors ->
            TypedResults.BadRequest(
                { ApiError.Code = ApiErrorCode.InvalidJsonBody
                  Message = NonEmptyString.fromString "Invalid request body."
                  Details =
                    errors
                    |> List.map (fun error ->
                        { ApiError.Code = ApiErrorCode.InvalidJsonBody
                          Message = NonEmptyString.fromString error
                          Details = List.empty }) }
            )
            :> IResult)

    let private handle createOrder replaceOrder request cancellationToken =
        Handlers.CreateOrReplace.handle
            createOrder
            replaceOrder
            Serialization.serializeOrder
            Common.tryGetId
            tryGetOrder
            request
            cancellationToken

    type CreateOrder = Order -> Async<Result<ETag, CreateError>>

    type ReplaceOrder = ETag -> Order -> Async<Result<ETag, ReplaceError>>

    let private handler =
        Func<CreateOrder, ReplaceOrder, HttpRequest, CancellationToken, Task<IResult>>(handle)

    [<RequireQualifiedAccess>]
    module internal Services =
        let private createOrder (provider: IServiceProvider) : CreateOrder =
            fun order ->
                Guid.NewGuid().ToString()
                |> NonEmptyString.fromString
                |> ETag.fromString
                |> Ok
                |> async.Return

        let private updateOrder (provider: IServiceProvider) : ReplaceOrder =
            fun eTag order -> Result.Ok eTag |> async.Return

        let configure (services: IServiceCollection) =
            services
                .AddSingleton<CreateOrder>(Func<IServiceProvider, CreateOrder>(createOrder))
                .AddSingleton<ReplaceOrder>(Func<IServiceProvider, ReplaceOrder>(updateOrder))
            |> ignore

    [<RequireQualifiedAccess>]
    module internal Endpoints =
        let configure (builder: IEndpointRouteBuilder) = builder.MapPut("/{id}", handler)

[<RequireQualifiedAccess>]
module Get =
    let private handle findOrder request cancellationToken =
        Handlers.Get.handle Common.tryGetId Serialization.serializeOrder findOrder request cancellationToken

    type FindOrder = OrderId -> Async<Option<Order * ETag>>

    let private handler =
        Func<FindOrder, HttpRequest, CancellationToken, Task<IResult>>(handle)

    [<RequireQualifiedAccess>]
    module internal Services =
        let private findOrder (provider: IServiceProvider) : FindOrder = fun orderId -> async.Return None

        let configure (services: IServiceCollection) =
            services.AddSingleton<FindOrder>(Func<IServiceProvider, FindOrder>(findOrder))
            |> ignore

    [<RequireQualifiedAccess>]
    module internal Endpoints =
        let configure (builder: IEndpointRouteBuilder) = builder.MapGet("/{id}", handler)

let configureServices services =
    CreateOrReplace.Services.configure services
    Get.Services.configure services

let configureEndpoints (builder: IVersionedEndpointRouteBuilder) =
    let groupBuilder =
        builder.MapGroup("/v{version:apiVersion}/orders").HasApiVersion(1)

    CreateOrReplace.Endpoints.configure groupBuilder |> ignore
    Get.Endpoints.configure groupBuilder
