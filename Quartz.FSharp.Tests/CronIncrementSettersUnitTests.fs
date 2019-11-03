module ``Unit tests for Cron module increment setters``

open NUnit.Framework
open FsUnit
open Quartz.Fsharp

[<Test>]
let ``Modifying seconds with valid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetSecondsWithIncrement 10 20

    match cron with
    | Ok cron ->
        cron.Seconds |> should equal "10/20"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying seconds with invalid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetSecondsWithIncrement 10 -5

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetSecondsWithIncrement -10 5

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


[<Test>]
let ``Modifying minutes with valid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetMinutesWithIncrement 10 20

    match cron with
    | Ok cron ->
        cron.Minutes |> should equal "10/20"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying minutes with invalid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetMinutesWithIncrement 90 120

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetMinutesWithIncrement 10 -5

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying hours with valid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetHoursWithIncrement 10 20

    match cron with
    | Ok cron ->
        cron.Hours |> should equal "10/20"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying hours with invalid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetHoursWithIncrement 40 50

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetHoursWithIncrement 9 -5

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying day of month with valid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetDayOfMonthWithIncrement 10 20

    match cron with
    | Ok cron ->
        cron.DayOfMonth |> should equal "10/20"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying day of month with invalid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetDayOfMonthWithIncrement 5 -10

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetDayOfMonthWithIncrement -10 -5

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying month with valid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetMonthWithIncrement Cron.January 2

    match cron with
    | Ok cron ->
        cron.Month |> should equal "1/2"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying month with invalid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetMonthWithIncrement Cron.January -10

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail



[<Test>]
let ``Modifying day of week with valid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetDayOfWeekWithIncrement Cron.Tuesday 4

    match cron with
    | Ok cron ->
        cron.DayOfWeek |> should equal "3/4"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying day of week with invalid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetDayOfWeekWithIncrement Cron.Tuesday -10

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


[<Test>]
let ``Modifying year with valid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetYearWithIncrement 1995 10

    match cron with
    | Ok cron ->
        cron.Year |> should equal "1995/10"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying year with invalid increment in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetYearWithIncrement 9999999 123213231

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetYearWithIncrement 1150 1190

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetYearWithIncrement -10 -5

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail
