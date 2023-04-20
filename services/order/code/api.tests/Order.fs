module api.tests.Order

open common
open FsCheck.FSharp
open FsCheck.Xunit
open FSharpPlus
open FsUnit.Xunit
open System
open System.Net
open System.Net.Http
open System.Net.Http.Json
open System.Text.Json.Nodes
open Microsoft.Extensions.DependencyInjection

[<RequireQualifiedAccess>]
module internal Gen =
    let defaultOf<'a> () = ArbMap.defaults.ArbFor<'a>().Generator

module CreateOrReplace =
    [<AutoOpen>]
    module Fixture =
        type private Message =
            | ContainsOrderId of OrderId * AsyncReplyChannel<bool>
            | AddOrder of Order * AsyncReplyChannel<Result<ETag, ApiErrorCode>>
            | UpdateOrder of ETag * Order * AsyncReplyChannel<Result<ETag, ApiErrorCode>>
            | ListOrders of AsyncReplyChannel<(Order * ETag) list>
            | Get of OrderId * AsyncReplyChannel<(Order * ETag)>

        type Fixture(initialOrders: seq<(Order * ETag)>) =
            let mutable orders =
                initialOrders
                |> Seq.map (fun (order, eTag) -> (order.Id, (order, eTag)))
                |> Map.ofSeq

            let agent =
                MailboxProcessor.Start(fun inbox ->
                    let rec loop () =
                        async {
                            let! message = inbox.Receive()

                            match message with
                            | ContainsOrderId(orderId, replyChannel) ->
                                replyChannel.Reply(Map.containsKey orderId orders)
                            | AddOrder(order, replyChannel) ->
                                if Map.containsKey order.Id orders then
                                    replyChannel.Reply(Error ApiErrorCode.ResourceAlreadyExists)
                                else
                                    let eTag = Guid.NewGuid().ToString() |> NonEmptyString.fromString |> ETag
                                    orders <- Map.add order.Id (order, eTag) orders
                                    replyChannel.Reply(Ok eTag)
                            | UpdateOrder(eTag, order, replyChannel) ->
                                match Map.tryFind order.Id orders with
                                | Some(existingOrder, existingETag) ->
                                    if existingETag = eTag then
                                        let newETag = Guid.NewGuid().ToString() |> NonEmptyString.fromString |> ETag
                                        let newOrderOption = Some(order, newETag)
                                        orders <- Map.change order.Id (konst newOrderOption) orders
                                        replyChannel.Reply(Ok newETag)
                                    else
                                        replyChannel.Reply(Error ApiErrorCode.ETagMismatch)
                                | None -> replyChannel.Reply(Error ApiErrorCode.ResourceNotFound)

                            | ListOrders replyChannel -> Map.values orders |> List.ofSeq |> replyChannel.Reply
                            | Get(orderId, replyChannel) -> Map.find orderId orders |> replyChannel.Reply

                            return! loop ()
                        }

                    loop ())

            let createOrder order =
                agent.PostAndAsyncReply(fun channel -> Message.AddOrder(order, channel))

            let updateOrder eTag order =
                agent.PostAndAsyncReply(fun channel -> Message.UpdateOrder(eTag, order, channel))

            let containsOrderId id =
                agent.PostAndReply(fun channel -> Message.ContainsOrderId(id, channel))

            member this.DoesNotContainOrderId(id) = containsOrderId id |> not

            member this.ListOrders() =
                agent.PostAndReply(fun channel -> Message.ListOrders channel)

            member this.Get(id) =
                agent.PostAndReply(fun channel -> Message.Get(id, channel))

            member this.CreateFactory() =
                let configureServices (services: IServiceCollection) =
                    services
                        .AddSingleton<api.Order.CreateOrReplace.CreateOrder>(createOrder)
                        .AddSingleton<api.Order.CreateOrReplace.UpdateOrder>(updateOrder)
                    |> ignore

                Factory.createWithServices configureServices

    let private orderToJsonObject (order: Order) =
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

    let private generateHttpRequest (order: Order) =
        let uri = $"/v1/orders/{OrderId.toString order.Id}"
        let message = new HttpRequestMessage(HttpMethod.Put, uri)
        let orderJson = orderToJsonObject order
        message.Content <- JsonContent.Create(orderJson, null, Json.serializerOptions)

        message

    let private generateValidCreateRequest (fixture: Fixture) =
        gen {
            let! order =
                Gen.defaultOf<Order> ()
                |> Gen.filter (fun order -> fixture.DoesNotContainOrderId(order.Id))

            let request = generateHttpRequest order
            request.Headers.TryAddWithoutValidation("If-None-Match", "*") |> ignore

            return request
        }

    let private generateValidUpdateRequest (fixture: Fixture) =
        gen {
            let! newOrder = Gen.defaultOf<Order> ()

            let! (updatedOrder, eTag) =
                Gen.elements (fixture.ListOrders())
                |> Gen.map (fun (existingOrder, eTag) -> ({ newOrder with Id = existingOrder.Id }, eTag))

            let request = generateHttpRequest updatedOrder

            request.Headers.TryAddWithoutValidation("If-Match", ETag.toString eTag)
            |> ignore

            return request
        }

    let private generateValidRequest fixture =
        [ generateValidCreateRequest fixture; generateValidUpdateRequest fixture ]
        |> Gen.oneof

    let private generateFixture () =
        let orderGen = Gen.defaultOf<Order> ()

        let eTagGen =
            Gen.defaultOf<Guid> ()
            |> Gen.map string
            |> Gen.map NonEmptyString.fromString
            |> Gen.map ETag

        Gen.zip orderGen eTagGen |> Gen.nonEmptyListOf |> Gen.map Fixture

    [<Property>]
    let ``Empty request body fails`` () =
        let arbitrary =
            gen {
                let! fixture = generateFixture ()
                let! request = generateValidRequest fixture
                request.Content <- null
                return (fixture, request)
            }
            |> Arb.fromGen

        Prop.forAll arbitrary (fun (fixture, request) ->
            async {
                // Arrange
                use factory = fixture.CreateFactory()
                use client = factory.CreateClient()
                let! cancellationToken = Async.CancellationToken

                // Act
                use! response = client.SendAsync(request, cancellationToken) |> Async.AwaitTask
                request.Dispose()

                // Assert
                use! responseStream = response.Content.ReadAsStreamAsync(cancellationToken) |> Async.AwaitTask
                let! json = JsonObject.fromStream responseStream
                let apiError = JsonNode.toObject<ApiError> json

                response.StatusCode |> should equal HttpStatusCode.BadRequest
                apiError.Code |> should equal ApiErrorCode.InvalidJsonBody
            })

    [<Property>]
    let ``Request ID must be a GUID`` () =
        let arbitrary =
            gen {
                let! fixture = generateFixture ()
                let! request = generateValidRequest fixture

                let! id =
                    Gen.defaultOf<char> ()
                    |> Gen.filter (fun x -> Char.IsLetterOrDigit(x) || x = '-')
                    |> Gen.nonEmptyListOf
                    |> Gen.map String.Concat

                request.RequestUri <- new Uri($"/v1/orders/{id}", UriKind.Relative)

                return (fixture, request)
            }
            |> Arb.fromGen

        Prop.forAll arbitrary (fun (fixture, request) ->
            async {
                // Arrange
                use factory = fixture.CreateFactory()
                use client = factory.CreateClient()
                let! cancellationToken = Async.CancellationToken

                // Act
                use! response = client.SendAsync(request, cancellationToken) |> Async.AwaitTask
                request.Dispose()

                // Assert
                use! responseStream = response.Content.ReadAsStreamAsync(cancellationToken) |> Async.AwaitTask
                let! json = JsonObject.fromStream responseStream
                let apiError = JsonNode.toObject<ApiError> json

                response.StatusCode |> should equal HttpStatusCode.BadRequest
                apiError.Code |> should equal ApiErrorCode.InvalidRouteValue
            })

    [<Property>]
    let ``Cannot pass both IfMatch and If-None-Match headers simultaneously`` () =
        let arbitrary =
            gen {
                let! fixture = generateFixture ()
                let! request = generateValidRequest fixture

                request.Headers.TryAddWithoutValidation("If-Match", Guid.NewGuid().ToString())
                |> ignore

                request.Headers.TryAddWithoutValidation("If-None-Match", "*") |> ignore
                return (fixture, request)
            }
            |> Arb.fromGen

        Prop.forAll arbitrary (fun (fixture, request) ->
            async {
                // Arrange
                use factory = fixture.CreateFactory()
                use client = factory.CreateClient()
                let! cancellationToken = Async.CancellationToken

                // Act
                use! response = client.SendAsync(request, cancellationToken) |> Async.AwaitTask
                request.Dispose()

                // Assert
                use! responseStream = response.Content.ReadAsStreamAsync(cancellationToken) |> Async.AwaitTask
                let! json = JsonObject.fromStream responseStream
                let apiError = JsonNode.toObject<ApiError> json

                response.StatusCode |> should equal HttpStatusCode.BadRequest
                apiError.Code |> should equal ApiErrorCode.InvalidConditionalHeader
            })

    [<Property>]
    let ``If-None-Match header must be wildcard`` () =
        let arbitrary =
            gen {
                let! fixture = generateFixture ()
                let! request = generateValidRequest fixture

                let! nonWildcardString =
                    Gen.defaultOf<string> ()
                    |> Gen.filter (fun x -> String.IsNullOrWhiteSpace(x) = false && x <> "*")

                request.Headers.Remove("If-Match") |> ignore

                request.Headers.TryAddWithoutValidation("If-None-Match", nonWildcardString)
                |> ignore

                return (fixture, request)
            }
            |> Arb.fromGen

        Prop.forAll arbitrary (fun (fixture, request) ->
            async {
                // Arrange
                use factory = fixture.CreateFactory()
                use client = factory.CreateClient()
                let! cancellationToken = Async.CancellationToken

                // Act
                use! response = client.SendAsync(request, cancellationToken) |> Async.AwaitTask
                request.Dispose()

                // Assert
                use! responseStream = response.Content.ReadAsStreamAsync(cancellationToken) |> Async.AwaitTask
                let! json = JsonObject.fromStream responseStream
                let apiError = JsonNode.toObject<ApiError> json

                response.StatusCode |> should equal HttpStatusCode.BadRequest
                apiError.Code |> should equal ApiErrorCode.InvalidConditionalHeader
            })

    [<Property>]
    let ``Must specify If-Match header or If-None-Match header`` () =
        let arbitrary =
            gen {
                let! fixture = generateFixture ()
                let! request = generateValidRequest fixture
                request.Headers.Remove("If-Match") |> ignore
                request.Headers.Remove("If-None-Match") |> ignore
                return (fixture, request)
            }
            |> Arb.fromGen

        Prop.forAll arbitrary (fun (fixture, request) ->
            async {
                // Arrange
                use factory = fixture.CreateFactory()
                use client = factory.CreateClient()
                let! cancellationToken = Async.CancellationToken

                // Act
                use! response = client.SendAsync(request, cancellationToken) |> Async.AwaitTask
                request.Dispose()

                // Assert
                use! responseStream = response.Content.ReadAsStreamAsync(cancellationToken) |> Async.AwaitTask
                let! json = JsonObject.fromStream responseStream
                let apiError = JsonNode.toObject<ApiError> json

                response.StatusCode |> should equal HttpStatusCode.PreconditionRequired
                apiError.Code |> should equal ApiErrorCode.InvalidConditionalHeader
            })

    [<Property>]
    let ``Cannot create an order with an ID that already exists`` () =
        let arbitrary =
            gen {
                let! fixture = generateFixture ()
                let! request = generateValidCreateRequest fixture

                let! existingOrderId =
                    fixture.ListOrders()
                    |> Gen.elements
                    |> Gen.map (fun (order, _) -> order.Id)
                    |> Gen.map OrderId.toString

                request.RequestUri <- new Uri($"/v1/orders/{existingOrderId}", UriKind.Relative)
                return (fixture, request)
            }
            |> Arb.fromGen

        Prop.forAll arbitrary (fun (fixture, request) ->
            async {
                // Arrange
                use factory = fixture.CreateFactory()
                use client = factory.CreateClient()
                let! cancellationToken = Async.CancellationToken

                // Act
                use! response = client.SendAsync(request, cancellationToken) |> Async.AwaitTask
                request.Dispose()

                // Assert
                use! responseStream = response.Content.ReadAsStreamAsync(cancellationToken) |> Async.AwaitTask
                let! json = JsonObject.fromStream responseStream
                let apiError = JsonNode.toObject<ApiError> json

                response.StatusCode |> should equal HttpStatusCode.Conflict
                apiError.Code |> should equal ApiErrorCode.ResourceAlreadyExists
            })


    [<Property>]
    let ``Can successfully create a new order`` () =
        let arbitrary =
            gen {
                let! fixture = generateFixture ()
                let! request = generateValidCreateRequest fixture
                return (fixture, request)
            }
            |> Arb.fromGen

        Prop.forAll arbitrary (fun (fixture, request) ->
            async {
                // Arrange
                use factory = fixture.CreateFactory()
                use client = factory.CreateClient()
                let! cancellationToken = Async.CancellationToken

                // Act
                use! response = client.SendAsync(request, cancellationToken) |> Async.AwaitTask
                request.Dispose()

                // Assert
                // Validate status code
                response.StatusCode |> should equal HttpStatusCode.Created

                // Validate response location
                response.Headers.Location.ToString()
                |> should equal (request.RequestUri.ToString())

                // Validate response eTag
                use! responseStream = response.Content.ReadAsStreamAsync(cancellationToken) |> Async.AwaitTask
                let! json = JsonObject.fromStream responseStream

                let orderId =
                    request.RequestUri
                    |> string
                    |> String.split [ "/" ]
                    |> Seq.last
                    |> OrderId.fromString

                let (order, fixtureETag) = fixture.Get(orderId)
                let responseETag = JsonObject.getStringProperty "eTag" json |> ETag.fromString
                responseETag |> should equal fixtureETag
            })
