namespace common

[<RequireQualifiedAccess>]
module Result =
    let coalesce result =
        match result with
        | Ok value -> value
        | Error value -> value

[<RequireQualifiedAccess>]
module AsyncResult =
    let map f asyncResult =
        async {
            let! result = asyncResult
            return Result.map f result
        }

    let coalesce asyncResult =
        async {
            let! result = asyncResult
            return Result.coalesce result
        }
