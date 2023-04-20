namespace common

open System.Net

type ETag =
    | ETag of NonEmptyString

    static member fromString value = NonEmptyString.fromString value |> ETag

    static member toString(ETag nonEmptyString) = NonEmptyString.toString nonEmptyString

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

type ApiErrorWithStatusCode =
    { Error: ApiError
      StatusCode: HttpStatusCode }

type ConditionalHeaderAction =
    | Create
    | Update of ETag
