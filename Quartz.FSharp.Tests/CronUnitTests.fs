module ``Unit tests for Cron module``

open NUnit.Framework
open FsUnit
open Quartz.Fsharp

[<Test>]
let ``Creating a default cron``() =
    let cron = Cron.CreateDefaultCron()

    cron.Seconds |> should equal "*"

    cron.Minutes |> should equal "*"

    cron.Hours |> should equal "*"

    cron.DayOfMonth |> should equal "?"

    cron.Month |> should equal "*"

    cron.DayOfWeek |> should equal "*"

    cron.Year |> should equal "*"

    cron
    |> Cron.ToString
    |> should equal "* * * ? * * *"

    cron
    |> Cron.ToString
    |> Cron.ValidateCronString
    |> should be True

[<Test>]
let ``Modifying seconds with valid value in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetSeconds 10
    let defaultCron = Cron.CreateDefaultCron()

    match cron with
    | Ok cron ->
        cron.Seconds |> should equal "10"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying seconds with invalid value in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetSeconds 90

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetSeconds -10

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


[<Test>]
let ``Modifying minutes with valid value in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetMinutes 10
    let defaultCron = Cron.CreateDefaultCron()

    match cron with
    | Ok cron ->
        cron.Minutes |> should equal "10"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying minutes with invalid value in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetMinutes 90

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetMinutes -10

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying hours with valid value in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetHours 10
    let defaultCron = Cron.CreateDefaultCron()

    match cron with
    | Ok cron ->
        cron.Hours |> should equal "10"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying hours with invalid value in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetHours 90

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetHours -10

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying day of month with valid value in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetDayOfMonth 10
    let defaultCron = Cron.CreateDefaultCron()

    match cron with
    | Ok cron ->
        cron.DayOfMonth |> should equal "10"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying day of month with invalid value in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetDayOfMonth 90

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetDayOfMonth -10

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying month with valid value in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetMonth Cron.January
    let defaultCron = Cron.CreateDefaultCron()

    match cron with
    | Ok cron ->
        cron.Month |> should equal "1"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying day of week with valid value in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetDayOfWeek Cron.Tuesday
    let defaultCron = Cron.CreateDefaultCron()

    match cron with
    | Ok cron ->
        cron.DayOfWeek |> should equal "3"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying year with valid value in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetYear 1995
    let defaultCron = Cron.CreateDefaultCron()

    match cron with
    | Ok cron ->
        cron.Year |> should equal "1995"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying year with invalid value in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetYear 9999999

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetYear 1150

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetYear -10

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail
