namespace api

open Microsoft.AspNetCore.Http

[<RequireQualifiedAccess>]
module internal Results =
    let validationProblem errors =
        let map = ("errors", Array.ofSeq errors) |> Seq.singleton |> Map.ofSeq
        Results.ValidationProblem(map)
