namespace api.tests

open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Mvc.Testing
open System

[<RequireQualifiedAccess>]
module Factory =
    let createWithoutServices () =
        new WebApplicationFactory<api.Program>()

    let createWithServices configureServices =
        { new WebApplicationFactory<api.Program>() with
            override _.ConfigureWebHost builder =
                let configureServicesAction = Action<IServiceCollection>(configureServices)
                builder.ConfigureServices(configureServicesAction) |> ignore }
