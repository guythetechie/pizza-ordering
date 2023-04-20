namespace api

open common
open FSharpPlus
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open System.Text.Json

[<RequireQualifiedAccess>]
module private Services =
    let private configureVersioning (services: IServiceCollection) =
        services
            .AddEndpointsApiExplorer()
            .AddApiVersioning(fun options -> options.ReportApiVersions <- true)
            .AddApiExplorer(fun options ->
                options.GroupNameFormat <- "'v'VVV"
                options.SubstituteApiVersionInUrl <- true)
            .EnableApiVersionBinding()
        |> ignore

        services

    let private configureSerialization (services: IServiceCollection) =
        services.ConfigureHttpJsonOptions(fun options ->
            options.SerializerOptions.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
            options.SerializerOptions.PropertyNameCaseInsensitive <- true

            Json.serializerOptions.Converters
            |> Seq.iter options.SerializerOptions.Converters.Add)

    let configure services =
        services
        |> configureVersioning
        |> configureSerialization
        |> Order.Create.Services.configure

[<RequireQualifiedAccess>]
module private WebApplicationBuilder =
    let create () =
        let builder = WebApplication.CreateBuilder()
        Services.configure builder.Services
        builder

[<RequireQualifiedAccess>]
module private WebApplication =
    let private configure (application: WebApplication) =
        let versionedBuilder = application.NewVersionedApi()
        Order.configureEndpoints versionedBuilder |> ignore
        application

    let create () =
        WebApplicationBuilder.create () |> (fun builder -> builder.Build()) |> configure

type Program() =
    class
    end

module Program =
    [<EntryPoint>]
    let main _ =
        WebApplication.create () |> fun application -> application.Run()
        0
