namespace api.tests

open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Mvc.Testing
open System

[<RequireQualifiedAccess>]
module Factory =
    let create configureServices =
        { new WebApplicationFactory<api.Program>() with
            override _.ConfigureWebHost builder =
                Action<IServiceCollection> configureServices
                |> builder.ConfigureServices
                |> ignore }