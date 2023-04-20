namespace api

open common
open FSharpPlus
open FSharpPlus.Data
open Microsoft.AspNetCore.Http
open System
open System.Net
open System.Text.Json.Nodes


[<RequireQualifiedAccess>]
module ApiErrorWithStatusCode =
    let toIResult (error: ApiErrorWithStatusCode) : IResult =
        TypedResults.Json(error.Error, statusCode = (int) error.StatusCode)

[<RequireQualifiedAccess>]
module internal Results =
    let validationProblem errors =
        let map = ("errors", Array.ofSeq errors) |> Seq.singleton |> Map.ofSeq
        Results.ValidationProblem(map)

[<RequireQualifiedAccess>]
module internal Handlers =
    [<RequireQualifiedAccess>]
    module CreateOrReplace =
        let private tryGetId (request: HttpRequest) =
            request.RouteValues["id"]
            |> string
            |> Guid.TryParse
            |> function
                | true, guid -> OrderId guid |> Ok
                | false, _ ->
                    Error
                        { ApiErrorWithStatusCode.Error =
                            { ApiError.Code = ApiErrorCode.InvalidRouteValue
                              Message = NonEmptyString.fromString "ID must be a valid GUID."
                              Details = List.empty }

                          StatusCode = HttpStatusCode.BadRequest }

        let private tryGetConditionalHeaderAction (request: HttpRequest) =
            match (request.Headers.TryGetValue("If-Match"), request.Headers.TryGetValue("If-None-Match")) with
            | (true, _), (true, _) ->
                Error
                    { ApiErrorWithStatusCode.Error =
                        { ApiError.Code = ApiErrorCode.InvalidConditionalHeader
                          Message =
                            NonEmptyString.fromString "Cannot specify both 'If-Match' and 'If-None-Match' headers."
                          Details = List.empty }

                      StatusCode = HttpStatusCode.BadRequest }

            | (true, eTag), (false, _) -> NonEmptyString.fromString eTag |> ETag |> ConditionalHeaderAction.Update |> Ok
            | (false, _), (true, ifMatchHeader) when String.Equals(ifMatchHeader, "*") ->
                Ok ConditionalHeaderAction.Create
            | (false, _), (true, _) ->
                Error
                    { ApiErrorWithStatusCode.Error =
                        { ApiError.Code = ApiErrorCode.InvalidConditionalHeader
                          Message = NonEmptyString.fromString "'If-None-Match' header must be '*'."
                          Details = List.empty }

                      StatusCode = HttpStatusCode.BadRequest }
            | (false, _), (false, _) ->
                Error
                    { ApiErrorWithStatusCode.Error =
                        { ApiError.Code = ApiErrorCode.InvalidConditionalHeader
                          Message = NonEmptyString.fromString "'If-Match' or 'If-None-Match' header must be specified."
                          Details = List.empty }

                      StatusCode = HttpStatusCode.PreconditionRequired }

        let private tryGetRequestBodyAsJsonObject (request: HttpRequest) =
            JsonObject.tryFromStream request.Body
            |> AsyncResult.setError
                { ApiErrorWithStatusCode.Error =
                    { ApiError.Code = ApiErrorCode.InvalidJsonBody
                      Message = NonEmptyString.fromString "Request body is not a JSON object."
                      Details = List.empty }

                  StatusCode = HttpStatusCode.BadRequest }

        let private tryGetRequestBodyFromJsonObject validateBodyJson jsonObject =
            validateBodyJson jsonObject
            |> Result.mapError (fun errors ->
                { ApiErrorWithStatusCode.Error =
                    { ApiError.Code = ApiErrorCode.InvalidJsonBody
                      Message = NonEmptyString.fromString "Request body is invalid."
                      Details =
                        errors
                        |> List.map (fun error ->
                            { ApiError.Code = ApiErrorCode.InvalidJsonBody
                              Message = NonEmptyString.fromString error
                              Details = List.empty }) }

                  StatusCode = HttpStatusCode.BadRequest })

        let private tryGetRequestBody validateBodyJson request =
            tryGetRequestBodyAsJsonObject request
            |> Async.map (Result.bind (tryGetRequestBodyFromJsonObject validateBodyJson))

        let private tryGetRequestParameters validateBodyJson parametersToResource request =
            asyncResultCE {
                let! requestBody = tryGetRequestBody validateBodyJson request
                and! id = tryGetId request
                and! headerAction = tryGetConditionalHeaderAction request

                let resource = parametersToResource id requestBody

                return (headerAction, resource)
            }

        let private processCreateRequest (requestUri: Uri) serializeResource createResource getResourceId resource =
            async {
                match! createResource resource with
                | Ok eTag ->
                    let (json: JsonObject) = serializeResource resource
                    json.Add("eTag", ETag.toString eTag)
                    return TypedResults.Created(requestUri, json) :> IResult
                | Error ApiErrorCode.ResourceAlreadyExists ->
                    return
                        TypedResults.Conflict(
                            { ApiError.Code = ApiErrorCode.ResourceAlreadyExists
                              Message =
                                NonEmptyString.fromString $"A resource with ID {getResourceId resource} already exists."
                              Details = List.empty }
                        )
                | _ -> return (NotImplementedException() |> raise)
            }

        let private processUpdateRequest serializeResource updateResource eTag getResourceId resource =
            async {
                match! updateResource eTag resource with
                | Ok eTag ->
                    let (json: JsonObject) = serializeResource resource
                    json.Add("eTag", ETag.toString eTag)
                    return TypedResults.Ok(json) :> IResult
                | Error ApiErrorCode.ResourceNotFound ->
                    return
                        TypedResults.NotFound(
                            { ApiError.Code = ApiErrorCode.ResourceNotFound
                              Message =
                                NonEmptyString.fromString $"A resource with ID {getResourceId resource} was not found."
                              Details = List.empty }
                        )
                | Error ApiErrorCode.ETagMismatch ->
                    return
                        TypedResults.Json(
                            { ApiError.Code = ApiErrorCode.ETagMismatch
                              Message =
                                NonEmptyString.fromString
                                    $"The If-Match header eTag doesn't match the resource's. Another process might have updated it. Please get a new ETag and try again."
                              Details = List.empty },
                            statusCode = (int) HttpStatusCode.PreconditionFailed
                        )
                | _ -> return (NotImplementedException() |> raise)
            }

        let private processRequest
            (requestUri: Uri)
            serializeResource
            createResource
            updateResource
            headerAction
            getResourceId
            resource
            =
            match headerAction with
            | ConditionalHeaderAction.Create ->
                processCreateRequest requestUri serializeResource createResource getResourceId resource
            | ConditionalHeaderAction.Update eTag ->
                processUpdateRequest serializeResource updateResource eTag getResourceId resource

        let handle
            createResource
            updateResource
            serializeResource
            getResourceId
            validateBodyJson
            parametersToResource
            request
            cancellationToken
            =
            async {
                match! tryGetRequestParameters validateBodyJson parametersToResource request with
                | Ok(headerAction, resource) ->
                    let requestUri =
                        new Uri($"{request.Scheme}://{request.Host}{request.PathBase}{request.Path}", UriKind.Absolute)

                    return!
                        processRequest
                            requestUri
                            serializeResource
                            createResource
                            updateResource
                            headerAction
                            getResourceId
                            resource
                | Error error -> return ApiErrorWithStatusCode.toIResult error
            }
            |> Async.startAsTaskWithCancellation cancellationToken
