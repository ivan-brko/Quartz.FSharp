namespace Quartz.Fsharp

module Helpers =
    type internal ResultBuilder() =
        member __.Return(x) = Ok x
        member __.ReturnFrom(x: Result<_, _>) = x
        member __.Bind(m, f) = Result.bind f m

    let internal result = ResultBuilder()

    let internal applyResult fResult xResult =
        match fResult, xResult with
        | Ok f, Ok x -> Ok(f x)
        | Error f, Ok x -> Error(f)
        | Ok f, Error x -> Error(x)
        | Error f, Error x -> Error x

    let internal traverseListOfResultsM f list =
        let cons head tail = head :: tail
        let initState = Ok []
        let folder head tail =
            result {
                let! h = f head
                let! t = tail
                return (cons h t) }
        List.foldBack folder list initState

    let inline internal nullValuesToOptions f n =
        if not (isNull n) then Some(f n)
        else None
