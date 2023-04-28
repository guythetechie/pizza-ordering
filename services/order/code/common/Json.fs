namespace common

open FSharpPlus
open System
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization
open System.IO

[<RequireQualifiedAccess>]
module Json =
    let serializerOptions =
        let serializerOptions = JsonFSharpOptions.Default().ToJsonSerializerOptions()
        serializerOptions.PropertyNameCaseInsensitive <- true
        serializerOptions.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        serializerOptions

    let deserializeStream<'a> (stream: Stream) =
        async {
            let! cancellationToken = Async.CancellationToken

            return!
                task { return! JsonSerializer.DeserializeAsync<'a>(stream, serializerOptions, cancellationToken) }
                |> Async.AwaitTask
        }

    let tryDeserializeStream<'a> stream =
        async {
            try
                let! result = deserializeStream<'a> stream

                return
                    match box result with
                    | null -> Error "Deserialized result is null."
                    | _ -> Ok result
            with
            | error when Exception.has<JsonException> error -> return Error error.Message
            | error -> return raise error
        }

[<RequireQualifiedAccess>]
module JsonValue =
    let tryAs<'a> (jsonValue: JsonValue) =
        match jsonValue.TryGetValue<'a>() with
        | true, value -> Some value
        | _ -> None

[<RequireQualifiedAccess>]
module JsonNode =
    let tryAsObject (jsonNode: JsonNode) =
        match jsonNode with
        | :? JsonObject as jsonObject -> Some jsonObject
        | _ -> None

    let tryAsArray (jsonNode: JsonNode) =
        match jsonNode with
        | :? JsonArray as jsonArray -> Some jsonArray
        | _ -> None

    let tryAsValue (jsonNode: JsonNode) =
        match jsonNode with
        | :? JsonValue as value -> Some value
        | _ -> None

    let tryAs<'a> (jsonNode: JsonNode) =
        tryAsValue jsonNode
        |> Option.bind (fun value ->
            match value.TryGetValue<'a>() with
            | true, x -> Some x
            | false, _ -> None)

    let fromStream stream =
        async {
            let! cancellationToken = Async.CancellationToken

            return!
                task {
                    return!
                        JsonSerializer.DeserializeAsync<JsonNode>(
                            stream,
                            options = Json.serializerOptions,
                            cancellationToken = cancellationToken
                        )
                }
                |> Async.AwaitTask
        }

[<RequireQualifiedAccess>]
module JsonObject =
    let private ifErrorThrow result =
        Result.defaultWith (fun error -> JsonException(error) |> raise) result

    let tryGetNullableProperty propertyName (jsonObject: JsonObject) =
        match jsonObject.TryGetPropertyValue(propertyName) with
        | true, node -> Ok node
        | false, _ -> Error $"Could not find property '{propertyName}' in JSON object."

    let tryGetProperty propertyName jsonObject =
        tryGetNullableProperty propertyName jsonObject
        |> Result.bind (fun node ->
            if isNull node then
                Error $"Property '{propertyName}' in JSON object is null."
            else
                Ok node)

    let tryGetStringProperty propertyName jsonObject =
        tryGetProperty propertyName jsonObject
        |> Result.map JsonNode.tryAs<string>
        |> Result.bind (function
            | Some value -> Ok value
            | None -> Error $"Property '{propertyName}' in JSON object is not a string.")

    let getStringProperty propertyName jsonObject =
        tryGetStringProperty propertyName jsonObject |> ifErrorThrow

    let tryGetGuidProperty propertyName jsonObject =
        tryGetProperty propertyName jsonObject
        |> Result.map JsonNode.tryAs<string>
        |> Result.bind (function
            | Some value ->
                match Guid.TryParse(value) with
                | true, guid -> Ok guid
                | false, _ -> Error $"Property '{propertyName}' in JSON object is not a GUID."
            | None -> Error $"Property '{propertyName}' in JSON object is not a GUID.")

    let tryGetJsonObjectProperty propertyName jsonObject =
        tryGetProperty propertyName jsonObject
        |> Result.map JsonNode.tryAsObject
        |> Result.bind (function
            | Some value -> Ok value
            | None -> Error $"Property '{propertyName}' in JSON object is not a JSON object.")

    let getJsonObjectProperty propertyName jsonObject =
        tryGetJsonObjectProperty propertyName jsonObject |> ifErrorThrow

    let tryGetJsonArrayProperty propertyName jsonObject =
        tryGetProperty propertyName jsonObject
        |> Result.map JsonNode.tryAsArray
        |> Result.bind (function
            | Some value -> Ok value
            | None -> Error $"Property '{propertyName}' in JSON object is not a JSON Array.")

    let getJsonArrayProperty propertyName jsonObject =
        tryGetJsonArrayProperty propertyName jsonObject |> ifErrorThrow

    let tryGetJsonObjectArrayProperty propertyName jsonObject =
        tryGetJsonArrayProperty propertyName jsonObject
        |> Result.map (traverse JsonNode.tryAsObject)
        |> Result.bind (function
            | Some(jsonObjects: JsonObject seq) -> Ok(List.ofSeq jsonObjects)
            | None -> Error $"Property '{propertyName}' in JSON object is not an array of JSON objects.")

    let getJsonObjectArrayProperty propertyName jsonObject =
        tryGetJsonObjectArrayProperty propertyName jsonObject |> ifErrorThrow

    let tryGetDateTimeOffsetProperty propertyName jsonObject =
        tryGetProperty propertyName jsonObject
        |> Result.map JsonNode.tryAs<DateTimeOffset>
        |> Result.bind (function
            | Some value -> Ok value
            | None -> Error $"Property '{propertyName}' in JSON object is not a valid date time offset.")

[<RequireQualifiedAccess>]
module JsonArray =
    let fromNodes nodes =
        nodes
        |> Seq.map (fun node -> node :> JsonNode)
        |> Array.ofSeq
        |> fun nodes -> new JsonArray(nodes)

    let fromObjects (jsonObjects: JsonObject seq) = fromNodes jsonObjects
