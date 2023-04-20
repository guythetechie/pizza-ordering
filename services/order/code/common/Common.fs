namespace common

open FSharpPlus
open FSharpPlus.Data
open System

[<RequireQualifiedAccess>]
module Async =
    let startAsTaskWithCancellation cancellationToken async =
        Async.StartAsTask(async, cancellationToken = cancellationToken)

[<RequireQualifiedAccess>]
module Result =
    let coalesce result =
        match result with
        | Ok value -> value
        | Error value -> value

    let setError value result = Result.mapError (konst value) result

[<RequireQualifiedAccess>]
module AsyncResult =
    let map f asyncResult =
        async {
            let! result = asyncResult
            return Result.map f result
        }

    let mapError f asyncResult =
        async {
            let! result = asyncResult
            return Result.mapError f result
        }

    let coalesce asyncResult =
        async {
            let! result = asyncResult
            return Result.coalesce result
        }

    let setError value asyncResult =
        Async.map (Result.setError value) asyncResult

    type Builder() =
        member inline this.Return(x) = Result.Ok x |> async.Return

        member inline this.ReturnFrom(x) : Async<Result<'a, 'b>> = x

        member inline this.Zero() = Result.Ok() |> async.Return

        member inline this.Bind(x, f) =
            async {
                match! x with
                | Ok ok -> return! f ok
                | Error error -> return Result.Error error
            }

        member inline this.Delay(f) : Async<Result<'a, 'b>> = async.Delay f

        member inline this.Combine(x1, x2) = this.Bind(x1, (fun () -> x2))

        member inline this.TryWith(x, handler) : Async<Result<'a, 'b>> = async.TryWith(x, handler)

        member inline this.TryFinally(x, compensation) : Async<Result<'a, 'b>> = async.TryFinally(x, compensation)

        member inline this.While(guard, x) =
            if guard () then
                let mutable whileAsync = Unchecked.defaultof<_>

                whileAsync <- this.Bind(x, (fun () -> if guard () then whileAsync else this.Zero()))

                whileAsync
            else
                this.Zero()


        member inline this.BindReturn(x, f) = map f x

        member inline this.MergeSources(x1, x2) =
            async {
                let! x1' = x1
                let! x2' = x2

                return
                    match (x1', x2') with
                    | Ok ok1, Ok ok2 -> Ok(ok1, ok2)
                    | Error error1, _ -> Error error1
                    | _, Error error2 -> Error error2
            }

        member inline this.Source(x) = this.ReturnFrom(x)

[<AutoOpen>]
module AsyncResultBuilderExtensions =
    type AsyncResult.Builder with

        member this.Source(x: Result<'a, 'b>) : Async<Result<'a, 'b>> = async.Return x

        member this.Source(x: Async<'a>) : Async<Result<'a, 'b>> = Async.map Result.Ok x


[<RequireQualifiedAccess>]
module Validation =
    let traverseBiApplyList f items =
        List.map f items
        |> Validation.partition
        |> function
            | successes, [] -> Success successes
            | _, errors -> List.concat errors |> Failure

    let mapFailure f validation =
        match validation with
        | Success success -> Success success
        | Failure failure -> f failure |> Failure

type NonEmptyString =
    private
    | NonEmptyString of string

    static member fromString value =
        if String.IsNullOrWhiteSpace(value) then
            invalidOp "String cannot be null, empty, or whitespace."
        else
            NonEmptyString value

    static member toString nonEmptyString =
        match nonEmptyString with
        | NonEmptyString value -> value

    override this.ToString() = NonEmptyString.toString this

    static member op_Implicit(nonEmptyString) = NonEmptyString.toString nonEmptyString

[<AutoOpen>]
module ComputationExpressions =
    let resultCE<'t, 'e> = MonadFxStrictBuilder<Result<'t, 'e>>()

    type ApplicativeValidationBuilder<'e, 't>() =
        inherit MonadFxStrictBuilder<Validation<'e, 't>>()
        member _.BindReturn(x, f: 'T -> 't) = Validation.map f x: Validation<'e, 't>

    let validationCE<'e, 't> = ApplicativeValidationBuilder<'e, 't>()

    let asyncResultCE = AsyncResult.Builder()

[<RequireQualifiedAccess>]
module Exception =
    let has<'a when 'a :> exn> (error: exn) =
        match error with
        | :? 'a -> true
        | :? AggregateException as aggregateException ->
            aggregateException.Flatten().InnerExceptions
            |> Seq.filter (fun innerError -> innerError :? 'a)
            |> Seq.isEmpty
            |> not
        | _ -> false
