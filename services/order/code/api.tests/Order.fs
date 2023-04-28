module api.tests.Order

open common
open FsCheck
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
open api
open System.IO

[<RequireQualifiedAccess>]
module internal Gen =
    let defaultOf<'a> () = ArbMap.defaults.ArbFor<'a>().Generator

module CreateOrReplace =
    [<AutoOpen>]
    module Fixture =
        type private Message =
            | ContainsOrderId of OrderId * AsyncReplyChannel<bool>
            | AddOrder of Order * AsyncReplyChannel<Result<ETag, CreateError>>
            | UpdateOrder of ETag * Order * AsyncReplyChannel<Result<ETag, ReplaceError>>
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
                                    replyChannel.Reply(Error CreateError.ResourceAlreadyExists)
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
                                        replyChannel.Reply(Error ReplaceError.EtagMismatch)
                                | None -> replyChannel.Reply(Error ReplaceError.ResourceNotFound)

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
                        .AddSingleton<api.Order.CreateOrReplace.ReplaceOrder>(updateOrder)
                    |> ignore

                Factory.createWithServices configureServices

    let private generateRequestUriFromIdString (id: string) =
        Uri($"/v1/orders/{id}", UriKind.Relative)

    let private generateRequestUri orderId =
        OrderId.toString orderId |> generateRequestUriFromIdString

    let private generateHttpRequest (order: Order) =
        let uri = generateRequestUri order.Id
        let message = new HttpRequestMessage(HttpMethod.Put, uri)
        let orderJson = api.Order.Serialization.serializeOrder order
        orderJson.Remove("id") |> ignore
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
                let! apiError = Json.deserializeStream<ApiError> responseStream

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

                request.RequestUri <- generateRequestUriFromIdString id

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
                let! apiError = Json.deserializeStream<ApiError> responseStream

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
                let! apiError = Json.deserializeStream<ApiError> responseStream

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
                let! apiError = Json.deserializeStream<ApiError> responseStream

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
                let! apiError = Json.deserializeStream<ApiError> responseStream

                response.StatusCode |> should equal HttpStatusCode.PreconditionRequired
                apiError.Code |> should equal ApiErrorCode.InvalidConditionalHeader
            })

    [<Property>]
    let ``Cannot create an order with an ID that already exists`` () =
        let arbitrary =
            gen {
                let! fixture = generateFixture ()
                let! request = generateValidCreateRequest fixture

                let! existingOrderId = fixture.ListOrders() |> Gen.elements |> Gen.map (fun (order, _) -> order.Id)

                request.RequestUri <- generateRequestUri existingOrderId
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
                let! apiError = Json.deserializeStream<ApiError> responseStream

                response.StatusCode |> should equal HttpStatusCode.Conflict
                apiError.Code |> should equal ApiErrorCode.ResourceAlreadyExists
            })

    let private getOrderFromRequest (request: HttpRequestMessage) =
        async {
            use memoryStream = new MemoryStream()
            let! cancellationToken = Async.CancellationToken
            do! request.Content.CopyToAsync(memoryStream, cancellationToken) |> Async.AwaitTask
            memoryStream.Position <- 0

            let! jsonObject = Json.deserializeStream<JsonObject> memoryStream
            let id = string request.RequestUri |> String.split["/"] |> Seq.last
            jsonObject.Add("id", id)

            return Order.Serialization.deserializeOrder jsonObject
        }

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
                let! requestOrder = getOrderFromRequest request

                // Act
                use! response = client.SendAsync(request, cancellationToken) |> Async.AwaitTask
                request.Dispose()

                // Assert
                // Validate status code
                response.StatusCode |> should equal HttpStatusCode.Created

                // Validate response location
                response.Headers.Location.ToString()
                |> should equal (request.RequestUri.ToString())

                // Validate response order
                use! responseStream = response.Content.ReadAsStreamAsync(cancellationToken) |> Async.AwaitTask
                let! json = Json.deserializeStream<JsonObject> responseStream
                let responseOrder = api.Order.Serialization.deserializeOrder json
                let (fixtureOrder, fixtureETag) = fixture.Get(responseOrder.Id)
                responseOrder |> should equal requestOrder
                responseOrder |> should equal fixtureOrder

                // Validate response ETag
                let responseETag = JsonObject.getStringProperty "eTag" json |> ETag.fromString
                responseETag |> should equal fixtureETag
            })

    [<Property>]
    let ``Cannot update an order with an invalid eTag`` () =
        let arbitrary =
            gen {
                let! fixture = generateFixture ()
                let! request = generateValidUpdateRequest fixture

                let! (existingOrder, existingOrderETag) = fixture.ListOrders() |> Gen.elements

                let badETag =
                    Gen.defaultOf<Guid> ()
                    |> Gen.map string
                    |> Gen.where (fun eTag -> eTag <> ETag.toString existingOrderETag)

                request.RequestUri <- generateRequestUri existingOrder.Id
                request.Headers.Remove("If-Match") |> ignore
                request.Headers.TryAddWithoutValidation("If-Match", string badETag) |> ignore

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
                let! apiError = Json.deserializeStream<ApiError> responseStream

                response.StatusCode |> should equal HttpStatusCode.PreconditionFailed
                apiError.Code |> should equal ApiErrorCode.ETagMismatch
            })

    [<Property>]
    let ``Cannot update an order that does not exist`` () =
        let arbitrary =
            gen {
                let! fixture = generateFixture ()
                let! request = generateValidUpdateRequest fixture

                let! nonExistingOrderId =
                    Gen.defaultOf<Guid> ()
                    |> Gen.map OrderId
                    |> Gen.where fixture.DoesNotContainOrderId

                request.RequestUri <- generateRequestUri nonExistingOrderId

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
                let! apiError = Json.deserializeStream<ApiError> responseStream

                response.StatusCode |> should equal HttpStatusCode.NotFound
                apiError.Code |> should equal ApiErrorCode.ResourceNotFound
            })

    [<Property>]
    let ``Can successfully update an order`` () =
        let arbitrary =
            gen {
                let! fixture = generateFixture ()
                let! request = generateValidUpdateRequest fixture
                return (fixture, request)
            }
            |> Arb.fromGen

        Prop.forAll arbitrary (fun (fixture, request) ->
            async {
                // Arrange
                use factory = fixture.CreateFactory()
                use client = factory.CreateClient()
                let! cancellationToken = Async.CancellationToken
                let! requestOrder = getOrderFromRequest request

                // Act
                use! response = client.SendAsync(request, cancellationToken) |> Async.AwaitTask
                request.Dispose()

                // Assert
                // Validate status code
                response.StatusCode |> should equal HttpStatusCode.OK

                // Validate response order
                use! responseStream = response.Content.ReadAsStreamAsync(cancellationToken) |> Async.AwaitTask
                let! json = Json.deserializeStream<JsonObject> responseStream
                let responseOrder = api.Order.Serialization.deserializeOrder json
                let (fixtureOrder, fixtureETag) = fixture.Get(responseOrder.Id)
                responseOrder |> should equal requestOrder
                responseOrder |> should equal fixtureOrder

                // Validate response ETag
                let responseETag = JsonObject.getStringProperty "eTag" json |> ETag.fromString
                responseETag |> should equal fixtureETag
            })

module Get =
    type private Fixture(orders: Map<OrderId, Order * ETag>) =
        let orders = orders

        let findOrder orderId =
            Map.tryFind orderId orders |> async.Return

        member this.Get(orderId) = Map.find orderId orders

        member this.ListOrderIds() = Map.keys orders

        member this.CreateFactory() =
            let configureServices (services: IServiceCollection) =
                services.AddSingleton<api.Order.Get.FindOrder>(findOrder) |> ignore

            Factory.createWithServices configureServices

    let private generateFixture () : Gen<Fixture> =
        let orderGen = Gen.defaultOf<Order> ()

        let eTagGen =
            Gen.defaultOf<Guid> ()
            |> Gen.map string
            |> Gen.map NonEmptyString.fromString
            |> Gen.map ETag

        Gen.zip orderGen eTagGen
        |> Gen.map (fun (order, eTag) -> (order.Id, (order, eTag)))
        |> Gen.nonEmptyListOf
        |> Gen.map Map.ofList
        |> Gen.map Fixture

    let private generateRequestUriFromIdString (orderId: string) =
        Uri($"/v1/orders/{orderId}", UriKind.Relative)

    let private generateRequestUri orderId =
        OrderId.toString orderId |> generateRequestUriFromIdString

    let private generateHttpRequest orderId =
        let uri = generateRequestUri orderId
        new HttpRequestMessage(HttpMethod.Get, uri)

    let private generateValidRequest (fixture: Fixture) =
        gen {
            let! orderId = fixture.ListOrderIds() |> Gen.elements
            return generateHttpRequest orderId
        }

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

                request.RequestUri <- generateRequestUriFromIdString id

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
                let! apiError = Json.deserializeStream<ApiError> responseStream

                response.StatusCode |> should equal HttpStatusCode.BadRequest
                apiError.Code |> should equal ApiErrorCode.InvalidRouteValue
            })

    [<Property>]
    let ``Cannot get an order that does not exist`` () =
        let arbitrary =
            gen {
                let! fixture = generateFixture ()
                let! request = generateValidRequest fixture

                let! nonExistingOrderId =
                    Gen.defaultOf<OrderId> ()
                    |> Gen.where (fun orderId -> fixture.ListOrderIds() |> Seq.contains orderId |> not)

                request.RequestUri <- generateRequestUri nonExistingOrderId
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
                let! apiError = Json.deserializeStream<ApiError> responseStream

                response.StatusCode |> should equal HttpStatusCode.NotFound
                apiError.Code |> should equal ApiErrorCode.ResourceNotFound
            })

    let private getOrderIdFromRequest (request: HttpRequestMessage) =
        string request.RequestUri |> String.split["/"] |> Seq.last |> OrderId.fromString

    [<Property>]
    let ``Can successfully get an order`` () =
        let arbitrary =
            gen {
                let! fixture = generateFixture ()
                let! request = generateValidRequest fixture
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
                response.StatusCode |> should equal HttpStatusCode.OK

                // Validate response order
                use! responseStream = response.Content.ReadAsStreamAsync(cancellationToken) |> Async.AwaitTask
                let! json = Json.deserializeStream<JsonObject> responseStream
                let responseOrder = api.Order.Serialization.deserializeOrder json
                let (fixtureOrder, fixtureETag) = fixture.Get(responseOrder.Id)
                responseOrder |> should equal fixtureOrder

                // Validate response ETag
                let responseETag = JsonObject.getStringProperty "eTag" json |> ETag.fromString
                responseETag |> should equal fixtureETag
            })
