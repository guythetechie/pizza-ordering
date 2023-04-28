namespace api

open common
open FSharp.Control
open FSharpPlus
open FSharpPlus.Data
open Microsoft.AspNetCore.Http
open System
open System.Net
open System.Text.Json.Nodes

type ContinuationToken =
    | ContinuationToken of NonEmptyString

    static member fromString value =
        NonEmptyString.fromString value |> ContinuationToken

    static member toString(ContinuationToken nonEmptyString) = NonEmptyString.toString nonEmptyString

    override this.ToString() = ContinuationToken.toString this

type PagedResult<'a> =
    { Value: 'a list; NextLink: Uri option }

[<RequireQualifiedAccess>]
module PagedResult =
    let serialize serializeResource (pagedResult: PagedResult<_>) =
        let json = JsonObject()
        json.Add("value", List.map serializeResource pagedResult.Value |> JsonArray.fromObjects)

        pagedResult.NextLink
        |> Option.iter (fun nextLink -> json.Add("nextLink", string nextLink))

        json

    let deserialize deserializeResource json =
        { PagedResult.Value =
            JsonObject.getJsonObjectArrayProperty "value" json
            |> Seq.map deserializeResource
            |> List.ofSeq
          NextLink =
            JsonObject.tryGetStringProperty "nextLink" json
            |> Result.map (fun nextLink -> Uri(nextLink, UriKind.RelativeOrAbsolute))
            |> Option.ofResult }

type PagedQueryParameters =
    { Skip: uint option
      Top: uint option
      MaxPageSize: uint option
      Select: Option<string list>
      ContinuationToken: ContinuationToken option }

    static member fromRequest(request: HttpRequest) =
        let queryCollection = request.Query

        let tryGetStringValuesQuery key =
            match queryCollection.TryGetValue(key) with
            | true, stringValues -> Some stringValues
            | false, _ -> None

        let tryGetStringQuery key =
            tryGetStringValuesQuery key |> Option.map string

        let tryGetUintQuery key =
            tryGetStringQuery key
            |> Option.bind (fun query ->
                match UInt32.TryParse(query) with
                | true, value -> Some value
                | false, _ -> None)

        { PagedQueryParameters.Skip = tryGetUintQuery "skip"
          Top = tryGetUintQuery "top"
          MaxPageSize = tryGetUintQuery "maxPageSize"
          Select =
            tryGetStringValuesQuery "select"
            |> Option.map (Seq.collect (String.split [ "," ]))
            |> Option.map (Seq.map String.trimWhiteSpaces)
            |> Option.map List.ofSeq
          ContinuationToken =
            tryGetStringQuery "continuationToken"
            |> Option.bind (fun query ->
                if String.IsNullOrWhiteSpace(query) then
                    None
                else
                    ContinuationToken.fromString query |> Some) }

[<RequireQualifiedAccess>]
module ApiErrorWithStatusCode =
    let toIResult (error: ApiErrorWithStatusCode) : IResult =
        TypedResults.Json(error.Error, statusCode = (int) error.StatusCode)

[<RequireQualifiedAccess>]
module internal Results =
    let validationProblem errors =
        let map = ("errors", Array.ofSeq errors) |> Seq.singleton |> Map.ofSeq
        Results.ValidationProblem(map)

type CreateError = | ResourceAlreadyExists

type ReplaceError =
    | ResourceNotFound
    | EtagMismatch

[<RequireQualifiedAccess>]
module internal Handlers =
    let private getIdFromRequest (request: HttpRequest) =
        String.split [ "/" ] request.Path.Value |> Seq.last

    let private getUriFromRequest (request: HttpRequest) =
        new Uri($"{request.Scheme}://{request.Host}{request.PathBase}{request.Path}", UriKind.Absolute)

    [<RequireQualifiedAccess>]
    module CreateOrReplace =
        let private tryGetConditionalHeaderAction (request: HttpRequest) =
            match (request.Headers.TryGetValue("If-Match"), request.Headers.TryGetValue("If-None-Match")) with
            | (true, _), (true, _) ->
                TypedResults.BadRequest(
                    { ApiError.Code = ApiErrorCode.InvalidConditionalHeader
                      Message = NonEmptyString.fromString "Cannot specify both 'If-Match' and 'If-None-Match' headers."
                      Details = List.empty }
                )
                :> IResult
                |> Error
            | (true, ifMatchValue), (false, _) when ifMatchValue.Count > 1 ->
                TypedResults.BadRequest(
                    { ApiError.Code = ApiErrorCode.InvalidConditionalHeader
                      Message = NonEmptyString.fromString "Can only specify one 'If-Match' header."
                      Details = List.empty }
                )
                :> IResult
                |> Error

            | (true, ifMatchValue), (false, _) ->
                NonEmptyString.fromString ifMatchValue
                |> ETag
                |> ConditionalHeaderAction.Update
                |> Ok
            | (false, _), (true, ifNoneMatchValue) when ifNoneMatchValue.Count > 1 ->
                TypedResults.BadRequest(
                    { ApiError.Code = ApiErrorCode.InvalidConditionalHeader
                      Message = NonEmptyString.fromString "Can only specify one 'If-None-Match' header."
                      Details = List.empty }
                )
                :> IResult
                |> Error
            | (false, _), (true, ifMatchHeader) when String.Equals(ifMatchHeader, "*") ->
                Ok ConditionalHeaderAction.Create
            | (false, _), (true, _) ->
                TypedResults.BadRequest(
                    { ApiError.Code = ApiErrorCode.InvalidConditionalHeader
                      Message = NonEmptyString.fromString "'If-None-Match' header must be '*'."
                      Details = List.empty }
                )
                :> IResult
                |> Error
            | (false, _), (false, _) ->
                TypedResults.Json(
                    { ApiError.Code = ApiErrorCode.InvalidConditionalHeader
                      Message = NonEmptyString.fromString "'If-Match' or 'If-None-Match' header must be specified."
                      Details = List.empty },
                    statusCode = StatusCodes.Status428PreconditionRequired
                )
                :> IResult
                |> Error

        let private tryGetRequestBodyAsJsonObject (request: HttpRequest) =
            Json.tryDeserializeStream<JsonObject> request.Body
            |> AsyncResult.setError (
                TypedResults.BadRequest(
                    { ApiError.Code = ApiErrorCode.InvalidJsonBody
                      Message = NonEmptyString.fromString "Request body is not a JSON object."
                      Details = List.empty }
                )
                :> IResult
            )

        let private processCreateRequest (requestUri: Uri) serializeResource createResource id resource =
            async {
                match! createResource resource with
                | Ok eTag ->
                    let (json: JsonObject) = serializeResource resource
                    json.Add("eTag", ETag.toString eTag)
                    return TypedResults.Created(requestUri, json) :> IResult
                | Error CreateError.ResourceAlreadyExists ->
                    return
                        TypedResults.Conflict(
                            { ApiError.Code = ApiErrorCode.ResourceAlreadyExists
                              Message = NonEmptyString.fromString $"A resource with ID {id} already exists."
                              Details = List.empty }
                        )
                        :> IResult
            }

        let private processUpdateRequest serializeResource replaceResource eTag id resource =
            async {
                match! replaceResource eTag resource with
                | Ok eTag ->
                    let (json: JsonObject) = serializeResource resource
                    json.Add("eTag", ETag.toString eTag)
                    return TypedResults.Ok(json) :> IResult
                | Error ReplaceError.ResourceNotFound ->
                    return
                        TypedResults.NotFound(
                            { ApiError.Code = ApiErrorCode.ResourceNotFound
                              Message = NonEmptyString.fromString $"A resource with ID {id} was not found."
                              Details = List.empty }
                        )
                        :> IResult
                | Error ReplaceError.EtagMismatch ->
                    return
                        TypedResults.Json(
                            { ApiError.Code = ApiErrorCode.ETagMismatch
                              Message =
                                NonEmptyString.fromString
                                    $"The If-Match header eTag doesn't match the resource's. Another process might have updated it. Please get a new ETag and try again."
                              Details = List.empty },
                            statusCode = (int) HttpStatusCode.PreconditionFailed
                        )
                        :> IResult
            }

        let private tryGetRequestParameters
            (tryGetResourceIdFromString: _ -> Result<_, _>)
            (tryGetResourceFromRequest: string -> JsonObject -> Result<_, _>)
            request
            =
            asyncResultCE {
                let! headerAction = tryGetConditionalHeaderAction request
                let idString = getIdFromRequest request
                let! id = tryGetResourceIdFromString idString
                let! requestBodyJson = tryGetRequestBodyAsJsonObject request
                let! resource = tryGetResourceFromRequest idString requestBodyJson
                return (headerAction, id, resource)
            }

        let private processRequest
            (requestUri: Uri)
            serializeResource
            createResource
            replaceResource
            headerAction
            id
            resource
            =
            match headerAction with
            | ConditionalHeaderAction.Create ->
                processCreateRequest requestUri serializeResource createResource id resource
            | ConditionalHeaderAction.Update eTag ->
                processUpdateRequest serializeResource replaceResource eTag id resource

        let handle
            createResource
            replaceResource
            serializeResource
            tryGetResourceIdFromString
            tryGetResourceFromRequest
            request
            cancellationToken
            =
            async {
                match! tryGetRequestParameters tryGetResourceIdFromString tryGetResourceFromRequest request with
                | Ok(headerAction, id, resource) ->
                    let requestUri = getUriFromRequest request

                    return!
                        processRequest
                            requestUri
                            serializeResource
                            createResource
                            replaceResource
                            headerAction
                            id
                            resource
                | Error error -> return error
            }
            |> Async.startAsTaskWithCancellation cancellationToken

    [<RequireQualifiedAccess>]
    module Get =
        let private tryGetResourceId tryGetResourceIdFromString request =
            let idString = getIdFromRequest request
            tryGetResourceIdFromString idString

        let private processRequest serializeResource findResourceById resourceId =
            async {
                match! findResourceById resourceId with
                | Some(resource, eTag) ->
                    let (json: JsonObject) = serializeResource resource
                    json.Add("eTag", ETag.toString eTag)
                    return TypedResults.Ok(json) :> IResult
                | None ->
                    return
                        TypedResults.NotFound(
                            { ApiError.Code = ApiErrorCode.ResourceNotFound
                              Message = NonEmptyString.fromString $"A resource with ID {resourceId} was not found."
                              Details = List.empty }
                        )
                        :> IResult
            }

        let handle tryGetResourceIdFromString serializeResource findResourceById request cancellationToken =
            match tryGetResourceId tryGetResourceIdFromString request with
            | Ok resourceId -> processRequest serializeResource findResourceById resourceId
            | Error error -> async.Return error
            |> Async.startAsTaskWithCancellation cancellationToken

    [<RequireQualifiedAccess>]
    module Delete =
        let private tryGetResourceId tryGetResourceIdFromString request =
            let idString = getIdFromRequest request
            tryGetResourceIdFromString idString

        let private processRequest deleteResource resourceId =
            async {
                do! deleteResource resourceId
                return TypedResults.NoContent() :> IResult
            }

        let handle tryGetResourceIdFromString deleteResource request cancellationToken =
            match tryGetResourceId tryGetResourceIdFromString request with
            | Ok resourceId -> processRequest deleteResource resourceId
            | Error error -> async.Return error
            |> Async.startAsTaskWithCancellation cancellationToken

    [<RequireQualifiedAccess>]
    module ListCollection =
        let private getPagedResults listResources queryParameters requestUri =
            async {
                let (results, continuationTokenOption) = listResources queryParameters

                let! values = AsyncSeq.toListAsync results

                let nextLinkOption =
                    continuationTokenOption
                    |> Option.map ContinuationToken.toString
                    |> Option.map (fun continuationToken ->
                        Uri.setQueryParameter "continuationToken" continuationToken requestUri)

                return
                    { PagedResult.Value = values
                      NextLink = nextLinkOption }
            }

        let private serializePagedResult serializeResource pagedResult =
            let serializeResourceAndETag (resource, eTag) =
                let (json: JsonObject) = serializeResource resource
                json.Add("eTag", ETag.toString eTag)
                json

            PagedResult.serialize serializeResourceAndETag pagedResult

        let private pagedResultToIResult propertiesToIncludeOption serializeResource pagedResult =
            let json = serializePagedResult serializeResource pagedResult

            let shouldRemoveProperty property =
                match propertiesToIncludeOption with
                | Some propertiesToInclude ->
                    propertiesToInclude
                    |> Seq.tryFind (fun propertyToInclude ->
                        String.Equals(propertyToInclude, property, StringComparison.OrdinalIgnoreCase))
                    |> Option.isSome
                | None -> false

            let propertiesToRemove =
                json
                |> Seq.map (fun x -> x.Key)
                |> Seq.filter shouldRemoveProperty
                |> List.ofSeq

            propertiesToRemove |> Seq.iter (json.Remove >> ignore)
            TypedResults.Ok(json) :> IResult

        let handle serializeResource listResources request cancellationToken =
            let queryParameters = PagedQueryParameters.fromRequest request
            let requestUri = getUriFromRequest request

            getPagedResults listResources queryParameters requestUri
            |> Async.map (pagedResultToIResult queryParameters.Select serializeResource)
            |> Async.startAsTaskWithCancellation cancellationToken
