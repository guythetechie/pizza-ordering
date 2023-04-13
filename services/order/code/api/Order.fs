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

[<RequireQualifiedAccess>]
module Create =
    type RequestDto =
        { Pizzas:
            {| Size: string
               Toppings: {| Topping: string; Amount: string |} list |} list
          PickupTime: string }

    let private toValidation (result: Result<_, string>) =
        result |> Result.mapError List.singleton |> Validation.ofResult

    let private validateRequestDto (dto: RequestDto) =
        let validateSize size =
            PizzaSize.tryFromString size |> toValidation

        let validateToppingType toppingType =
            Topping.tryFromString toppingType |> toValidation

        let validateToppingAmount amount =
            ToppingAmount.tryFromString amount |> toValidation

        let validateTopping (topping: {| Topping: string; Amount: string |}) =
            applicative {
                let! toppingType = validateToppingType topping.Topping
                and! toppingAmount = validateToppingAmount topping.Amount
                return (toppingType, toppingAmount)
            }

        let flattenValidations validations =
            validations
            |> List.fold
                (fun state validation ->
                    applicative {
                        let! stateItems = state
                        and! topping = validation
                        return List.cons topping stateItems
                    })
                (Validation.Success List.empty)

        let validateToppings toppings =
            toppings |> List.map validateTopping |> flattenValidations

        let validatePizza
            (pizza:
                {| Size: string
                   Toppings: {| Topping: string; Amount: string |} list |})
            =
            applicative {
                let! size = validateSize pizza.Size
                and! toppings = validateToppings pizza.Toppings

                return
                    { Pizza.Size = size
                      Toppings = toppings }
            }

        let validatePizzas pizzas =
            match pizzas with
            | [] -> List.singleton "At least one pizza must be specified." |> Failure
            | _ -> pizzas |> List.map validatePizza |> flattenValidations

        let validatePickupTime (value: string) =
            match DateTimeOffset.TryParse value with
            | true, parsedValue -> PickupTime parsedValue |> Validation.Success
            | false, _ -> List.singleton $"'{value}' is not a valid pickup time." |> Validation.Failure

        let validateDtoIsNotNull () =
            match box dto with
            | null -> Result.Error "Request cannot be null." |> toValidation
            | _ -> Validation.Success()

        let validateNonNullDto () =
            applicative {
                let! pizzas = validatePizzas dto.Pizzas
                and! pickupTime = validatePickupTime dto.PickupTime

                return
                    { Order.Pizzas = pizzas
                      PickupTime = pickupTime }
            }

        validateDtoIsNotNull ()
        |> Validation.bind validateNonNullDto
        |> Validation.toResult
        |> Result.mapError Results.validationProblem

    type private PersistOrder = Order -> Async<unit>

    let private createSuccessfulResponse () = Results.CreatedAtRoute("/")

    let private handle persistOrder requestDto =
        validateRequestDto requestDto
        |> traverse persistOrder
        |> AsyncResult.map createSuccessfulResponse
        |> AsyncResult.coalesce
        |> Async.StartAsTask

    let private handler = Func<PersistOrder, RequestDto, Task<IResult>>(handle)

    [<RequireQualifiedAccess>]
    module Services =
        let private persistOrder =
            Func<IServiceProvider, PersistOrder>(fun provider -> fun _ -> async.Return())

        let configure (services: IServiceCollection) =
            services.AddSingleton<PersistOrder>(persistOrder) |> ignore

    [<RequireQualifiedAccess>]
    module Endpoints =
        let configure (builder: IEndpointRouteBuilder) =
            builder
                .MapPost("/", handler)
                .WithOpenApi(fun operation ->
                    operation.Description <- "Place an order"
                    operation)
                .ProducesProblem(StatusCodes.Status400BadRequest)
                .ProducesProblem(StatusCodes.Status201Created)
            |> ignore

let configureEndpoints (builder: IVersionedEndpointRouteBuilder) =
    let groupBuilder =
        builder
            .MapGroup("/v{version:apiVersion}/orders")
            .HasApiVersion(1)
            .WithTags("Blobs")

    Create.Endpoints.configure groupBuilder
