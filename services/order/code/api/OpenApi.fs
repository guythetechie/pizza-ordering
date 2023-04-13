[<RequireQualifiedAccess>]
module internal api.OpenApi

open Asp.Versioning
open Asp.Versioning.ApiExplorer
open Asp.Versioning.Builder
open FSharpPlus
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Mvc.ApiExplorer
open Microsoft.AspNetCore.Mvc.ModelBinding
open Microsoft.AspNetCore.Routing
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Options
open Microsoft.OpenApi.Models
open Swashbuckle.AspNetCore.SwaggerGen
open System
open System.Linq
open System.Text
open System.Text.Json

[<RequireQualifiedAccess>]
module Services =
    let private isNotNull x = isNull x |> not

    type private ConfigureSwaggerOptions(provider: IApiVersionDescriptionProvider) =

        let provider = provider

        let createDescriptionOpenApiInfo (description: ApiVersionDescription) =
            let text =
                new StringBuilder("An example application with OpenAPI, Swashbuckle, and API versioning.")

            let info =
                let info = OpenApiInfo()
                info.Title <- "Example API"
                info.Version <- description.ApiVersion.ToString()
                info

            if description.IsDeprecated then
                text.Append(" This API version has been deprecated.") |> ignore

            if isNotNull description.SunsetPolicy then
                let policy = description.SunsetPolicy

                if policy.Date.HasValue then
                    let policyDate = policy.Date.Value

                    text
                        .Append("The API will be sunset on ")
                        .Append(policyDate.Date.ToShortDateString())
                        .Append('.')
                    |> ignore

                if policy.HasLinks then
                    text.AppendLine() |> ignore

                    policy.Links
                    |> Seq.iter (fun link ->
                        if link.Type.HasValue && link.Type.Value = "text/html" then
                            text.AppendLine() |> ignore

                            if link.Title.HasValue then
                                text.Append(link.Title.Value).Append(": ") |> ignore

                            text.Append(link.LinkTarget.OriginalString) |> ignore)

            info.Description <- text.ToString()
            info

        interface IConfigureOptions<SwaggerGenOptions> with
            member this.Configure(options: SwaggerGenOptions) =
                provider.ApiVersionDescriptions
                |> Seq.iter (fun description ->
                    options.SwaggerDoc(description.GroupName, createDescriptionOpenApiInfo description))

    type private SwaggerDefaultValues() =
        interface IOperationFilter with
            member this.Apply(operation, context) =
                let apiDescription = context.ApiDescription
                operation.Deprecated <- operation.Deprecated || apiDescription.IsDeprecated()

                context.ApiDescription.SupportedResponseTypes
                |> Seq.iter (fun responseType ->
                    let responseKey =
                        if responseType.IsDefaultResponse then
                            "default"
                        else
                            responseType.StatusCode.ToString()

                    let response = operation.Responses[responseKey]

                    response.Content.Keys
                    |> Seq.iter (fun contentType ->
                        if
                            responseType.ApiResponseFormats
                            |> Seq.exists (fun responseFormat -> responseFormat.MediaType = contentType)
                            |> not
                        then
                            response.Content.Remove(contentType) |> ignore))

                if isNotNull operation.Parameters then
                    operation.Parameters
                    |> Seq.iter (fun parameter ->
                        let description =
                            apiDescription.ParameterDescriptions.First(fun x -> x.Name = parameter.Name)

                        parameter.Description <-
                            match (parameter.Description, description.ModelMetadata) with
                            | (null, null) -> null
                            | (null, metadata) -> metadata.Description
                            | _ -> parameter.Description

                        if
                            isNull parameter.Schema.Default
                            && isNotNull description.DefaultValue
                            && description.DefaultValue :? DBNull = false
                            && isNotNull description.ModelMetadata
                        then
                            let json =
                                JsonSerializer.Serialize(description.DefaultValue, description.ModelMetadata.ModelType)

                            parameter.Schema.Default <- OpenApiAnyFactory.CreateFromJson(json)

                        parameter.Required <- parameter.Required || description.IsRequired)

    let configure (services: IServiceCollection) =
        services
            .AddEndpointsApiExplorer()
            .AddApiVersioning(fun options -> options.ReportApiVersions <- true)
            .AddApiExplorer(fun options ->
                options.GroupNameFormat <- "'v'VVV"
                options.SubstituteApiVersionInUrl <- true)
            .EnableApiVersionBinding()
        |> ignore

        services
            .AddTransient<IConfigureOptions<SwaggerGenOptions>, ConfigureSwaggerOptions>()
            .AddSwaggerGen(fun options ->
                options.OperationFilter<SwaggerDefaultValues>()
                options.CustomSchemaIds(fun x -> x.FullName.Replace('+', '_')))

[<RequireQualifiedAccess>]
module WebApplication =
    let private configureSwagger (builder: 'a when 'a :> IApplicationBuilder and 'a :> IEndpointRouteBuilder) =
        builder
            .UseSwagger()
            .UseSwaggerUI(fun options ->
                builder.DescribeApiVersions()
                |> Seq.iter (fun description ->
                    let url = $"/swagger/{description.GroupName}/swagger.json"
                    let name = description.GroupName.ToUpperInvariant()
                    options.SwaggerEndpoint(url, name)))

    let configure (builder: 'a when 'a :> IApplicationBuilder and 'a :> IEndpointRouteBuilder) =
        let versionedBuilder = builder.NewVersionedApi()
        Order.configureEndpoints versionedBuilder
        configureSwagger builder |> ignore

//internal static class WebApplication
//{
//    public static void Configure<T>(T builder) where T : IApplicationBuilder, IEndpointRouteBuilder
//    {
//        var versionedRouteBuilder = builder.NewVersionedApi();
//        MapEndpoints(versionedRouteBuilder);
//        ConfigureSwagger(builder);
//    }

//    private static void MapEndpoints(IVersionedEndpointRouteBuilder builder)
//    {
//        Blobs.Endpoints.Map(builder);
//        Installations.Endpoints.Map(builder);
//    }

//    private static void ConfigureSwagger<T>(T builder) where T : IApplicationBuilder, IEndpointRouteBuilder
//    {
//        builder.UseSwagger();
//        builder.UseSwaggerUI(options =>
//        {
//            var descriptions = builder.DescribeApiVersions();

//            // build a swagger endpoint for each discovered API version
//            foreach (var description in descriptions)
//            {
//                var url = $"/swagger/{description.GroupName}/swagger.json";
//                var name = description.GroupName.ToUpperInvariant();
//                options.SwaggerEndpoint(url, name);
//            }
//        });
//    }
//}
