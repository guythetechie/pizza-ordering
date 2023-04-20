namespace common

open FSharpPlus
open System
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization

[<RequireQualifiedAccess>]
module Json =
    let serializerOptions =
        let serializerOptions = JsonFSharpOptions.Default().ToJsonSerializerOptions()
        serializerOptions.PropertyNameCaseInsensitive <- true
        serializerOptions.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        serializerOptions

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

    let tryAsString node =
        tryAsValue node |> Option.bind JsonValue.tryAs<string>

    let tryAsDateTimeOffset node =
        tryAsValue node |> Option.bind JsonValue.tryAs<DateTimeOffset>

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

    let tryFromStream stream =
        async {
            let errorMessage = "Failed to deserialize JSON."

            try
                let! result = fromStream stream
                return if isNull result then Error errorMessage else Ok result
            with
            | error when Exception.has<JsonException> error -> return Error errorMessage
            | error -> return raise error
        }

    let tryFromObject<'a> (input: 'a) =
        JsonSerializer.SerializeToNode(input, Json.serializerOptions)
        |> function
            | null -> Error("Failed to serialize object.")
            | node -> Ok node

    let toObject<'a> (node: JsonNode) =
        JsonSerializer.Deserialize<'a>(node, Json.serializerOptions)

    let tryToObject<'a> node =
        try
            toObject<'a> node |> Ok
        with
        | error when Exception.has<JsonException> error -> Error("Failed to deserialize JSON node.")
        | _ -> reraise ()

[<RequireQualifiedAccess>]
module JsonObject =
    let fromStream stream =
        async {
            let! node = JsonNode.fromStream stream
            return node.AsObject()
        }

    let tryFromStream stream =
        JsonNode.tryFromStream stream
        |> AsyncResult.map JsonNode.tryAsObject
        |> Async.map (
            Result.bind (function
                | Some value -> Ok value
                | None -> Error "Stream contents is not a JSON object.")
        )

    let tryFromObject<'a> (input: 'a) =
        JsonNode.tryFromObject<'a> input
        |> Result.map JsonNode.tryAsObject
        |> Result.map (function
            | Some value -> Ok value
            | None -> Error "Failed to serialize object to JSON object.")

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
        |> Result.map JsonNode.tryAsString
        |> Result.bind (function
            | Some value -> Ok value
            | None -> Error $"Property '{propertyName}' in JSON object is not a string.")

    let private defaultResultWithJsonException result =
        result |> Result.defaultWith (fun error -> JsonException(error) |> raise)

    let getStringProperty propertyName jsonObject =
        tryGetStringProperty propertyName jsonObject |> defaultResultWithJsonException

    let tryGetJsonObjectProperty propertyName jsonObject =
        tryGetProperty propertyName jsonObject
        |> Result.map JsonNode.tryAsObject
        |> Result.bind (function
            | Some value -> Ok value
            | None -> Error $"Property '{propertyName}' in JSON object is not a JSON object.")

    let tryGetJsonArrayProperty propertyName jsonObject =
        tryGetProperty propertyName jsonObject
        |> Result.map JsonNode.tryAsArray
        |> Result.bind (function
            | Some value -> Ok value
            | None -> Error $"Property '{propertyName}' in JSON object is not a JSON Array.")

    let tryGetJsonObjectArrayProperty propertyName jsonObject =
        tryGetJsonArrayProperty propertyName jsonObject
        |> Result.map (traverse JsonNode.tryAsObject)
        |> Result.bind (function
            | Some(jsonObjects: JsonObject seq) -> Ok(List.ofSeq jsonObjects)
            | None -> Error $"Property '{propertyName}' in JSON object is not an array of JSON objects.")

    let tryGetDateTimeOffsetProperty propertyName jsonObject =
        tryGetProperty propertyName jsonObject
        |> Result.map JsonNode.tryAsDateTimeOffset
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
