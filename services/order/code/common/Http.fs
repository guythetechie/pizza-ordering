namespace common

open Flurl
open System
open System.Net
open System.Text.Json.Nodes

type ETag =
    | ETag of NonEmptyString

    static member fromString value = NonEmptyString.fromString value |> ETag

    static member toString(ETag nonEmptyString) = NonEmptyString.toString nonEmptyString

    static member generate() =
        Guid.NewGuid() |> string |> ETag.fromString

type ApiErrorCode =
    | ResourceNotFound
    | ResourceAlreadyExists
    | InvalidConditionalHeader
    | InvalidJsonBody
    | InvalidRouteValue
    | ETagMismatch

type ApiError =
    { Code: ApiErrorCode
      Message: NonEmptyString
      Details: ApiError list }

    static member serialize(error: ApiError) =
        let jsonObject =
            [ ("code",
               match error.Code with
               | ResourceNotFound -> nameof ResourceNotFound
               | ResourceAlreadyExists -> nameof ResourceAlreadyExists
               | InvalidConditionalHeader -> nameof InvalidConditionalHeader
               | InvalidJsonBody -> nameof InvalidJsonBody
               | InvalidRouteValue -> nameof InvalidRouteValue
               | ETagMismatch -> nameof ETagMismatch
               |> JsonNode.op_Implicit)
              ("message", NonEmptyString.toString error.Message |> JsonNode.op_Implicit) ]
            |> Map.ofSeq
            |> JsonObject

        if Seq.isEmpty error.Details = false then
            let details = Seq.map ApiError.serialize error.Details |> JsonArray.fromNodes
            jsonObject.Add("details", details)

        JsonObject([ ("error", jsonObject :> JsonNode) ] |> Map.ofSeq)

    static member deserialize jsonObject =
        let errorJson = JsonObject.getJsonObjectProperty "error" jsonObject

        { ApiError.Code =
            match JsonObject.getStringProperty "code" errorJson with
            | nameof ResourceNotFound -> ResourceNotFound
            | nameof ResourceAlreadyExists -> ResourceAlreadyExists
            | nameof InvalidConditionalHeader -> InvalidConditionalHeader
            | nameof InvalidJsonBody -> InvalidJsonBody
            | nameof InvalidRouteValue -> InvalidRouteValue
            | nameof ETagMismatch -> ETagMismatch
            | value -> invalidOp $"'{value}' is not a valid API error code."
          Message = JsonObject.getStringProperty "message" errorJson |> NonEmptyString.fromString
          Details =
            JsonObject.tryGetJsonObjectArrayProperty "details" errorJson
            |> Result.map (List.map ApiError.deserialize)
            |> Result.defaultValue List.empty }


type ApiErrorWithStatusCode =
    { Error: ApiError
      StatusCode: HttpStatusCode }

type ConditionalHeaderAction =
    | Create
    | Update of ETag

[<RequireQualifiedAccess>]
module Uri =
    let setQueryParameter key value (uri: Uri) = uri.SetQueryParam(key, value).ToUri()
