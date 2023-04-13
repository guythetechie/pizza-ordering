module api.tests.Order

open System.Net
open Xunit
open FsUnit.Xunit

module Create =
    [<Fact>]
    let ``Empty request body fails`` () =
        task {
            use factory = Factory.create ignore
            use client = factory.CreateClient()
            use! response = client.PostAsync("/v1/orders", null)

            response.StatusCode |> should equal HttpStatusCode.BadRequest
        }
