module ``Unit tests for Cron module range setters``

open NUnit.Framework
open FsUnit
open Quartz.Fsharp

[<Test>]
let ``Modifying seconds with valid range in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetSecondsRange 10 20

    match cron with
    | Ok cron ->
        cron.Seconds |> should equal "10-20"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying seconds with invalid range in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetSecondsRange 10 90

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetSecondsRange -10 -5

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


[<Test>]
let ``Modifying minutes with valid range in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetMinutesRange 10 20

    match cron with
    | Ok cron ->
        cron.Minutes |> should equal "10-20"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying minutes with invalid range in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetMinutesRange 90 120

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetMinutesRange -10 -5

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying hours with valid range in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetHoursRange 10 20

    match cron with
    | Ok cron ->
        cron.Hours |> should equal "10-20"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying hours with invalid range in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetHoursRange 40 50

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetHoursRange -10 -5

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying day of month with valid range in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetDayOfMonthRange 10 20

    match cron with
    | Ok cron ->
        cron.DayOfMonth |> should equal "10-20"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying day of month with invalid range in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetDayOfMonthRange 90 120

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetDayOfMonthRange -10 -5

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying month with valid range in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetMonthRange Cron.January Cron.March

    match cron with
    | Ok cron ->
        cron.Month |> should equal "1-3"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying day of week with valid range in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetDayOfWeekRange Cron.Tuesday Cron.Friday

    match cron with
    | Ok cron ->
        cron.DayOfWeek |> should equal "3-6"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying year with valid range in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetYearRange 1995 1998

    match cron with
    | Ok cron ->
        cron.Year |> should equal "1995-1998"
        cron
        |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True
    | Error _ -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail

[<Test>]
let ``Modifying year with invalid range in default cron``() =
    let cron = Cron.CreateDefaultCron() |> Cron.SetYearRange 9999999 123213231

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetYearRange 1150 1190

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail


    let cron = Cron.CreateDefaultCron() |> Cron.SetYearRange -10 -5

    match cron with
    | Ok cron -> true |> should be False //todo, this is ugly, check if we can somehow else make this case always fail
    | Error _ -> true |> should be True //todo, this is ugly, check if we can somehow else make this case always fail
