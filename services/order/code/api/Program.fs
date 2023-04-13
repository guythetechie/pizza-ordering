namespace api

open FSharpPlus
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open System.Text.Json
open System.Text.Json.Serialization

[<RequireQualifiedAccess>]
module private Services =
    let private configureSerialization (services: IServiceCollection) =
        let serializerOptions = JsonFSharpOptions.Default().ToJsonSerializerOptions()

        services.ConfigureHttpJsonOptions(fun options ->
            options.SerializerOptions.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
            options.SerializerOptions.PropertyNameCaseInsensitive <- true

            serializerOptions.Converters
            |> Seq.iter options.SerializerOptions.Converters.Add)

    let configure services =
        services
        |> configureSerialization
        |> OpenApi.Services.configure
        |> api.Order.Create.Services.configure

[<RequireQualifiedAccess>]
module private WebApplicationBuilder =
    let create () =
        let builder = WebApplication.CreateBuilder()
        Services.configure builder.Services
        builder


[<RequireQualifiedAccess>]
module private WebApplication =
    let create () =
        WebApplicationBuilder.create ()
        |> fun builder -> builder.Build()
        |> tap OpenApi.WebApplication.configure

type Program() =
    class
    end

module Program =
    [<EntryPoint>]
    let main _ =
        WebApplication.create () |> fun application -> application.Run()
        0
