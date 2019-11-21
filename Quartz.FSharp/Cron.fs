namespace Quartz.Fsharp

module Cron =
    open Quartz.Fsharp.Helpers

    type Cron =
        { Seconds: string
          Minutes: string
          Hours: string
          DayOfMonth: string
          Month: string
          DayOfWeek: string
          Year: string }

    type CronBuilderError =
        | SecondsOutOfRange
        | SecondsIncrementOutOfRange
        | MinutesOutOfRange
        | MinutesIncrementOutOfRange
        | HoursOutOfRange
        | HoursIncrementOutOfRange
        | DayOfMonthOutOfRange
        | DayOfMonthIncrementOutOfRange
        | MonthOutOfRange
        | MonthIncrementOutOfRange
        | DayOfWeekOutOfRange
        | DayOfWeekIncrementOutOfRange
        | YearOutOfRange
        | YearIncrementOutOfRange
        | CronStringInvalid

    type Month =
        | Numeric of int
        | January
        | February
        | March
        | April
        | May
        | June
        | July
        | August
        | September
        | October
        | November
        | December

    type DayOfWeek =
        | Numeric of int
        | Monday
        | Tuesday
        | Wednesday
        | Thursday
        | Friday
        | Saturday
        | Sunday

    let private getNumericDay dayOfWeek =
        match dayOfWeek with
        | Monday -> Ok 2
        | Tuesday -> Ok 3
        | Wednesday -> Ok 4
        | Thursday -> Ok 5
        | Friday -> Ok 6
        | Saturday -> Ok 7
        | Sunday -> Ok 1
        | Numeric n ->
            if (n < 1 || n > 7) then Error DayOfWeekOutOfRange
            else Ok n

    let private getNumericMonth month =
        match month with
        | January -> Ok 1
        | February -> Ok 2
        | March -> Ok 3
        | April -> Ok 4
        | May -> Ok 5
        | June -> Ok 6
        | July -> Ok 7
        | August -> Ok 8
        | September -> Ok 9
        | October -> Ok 10
        | November -> Ok 11
        | December -> Ok 12
        | Month.Numeric n ->
            if (n < 1 || n > 12) then Error MonthOutOfRange
            else Ok n

    let private commonIncrementValidation increment =
        if increment > 0 then Some increment
        else None

    let private validateSeconds secs =
        if (secs < 0 || secs > 59) then Error SecondsOutOfRange
        else Ok secs

    let private validateSecondsIncrement increment =
        match commonIncrementValidation increment with
        | Some increment -> Ok increment
        | None -> Error SecondsIncrementOutOfRange

    let private validateMinutes minutes =
        if (minutes < 0 || minutes > 59) then Error MinutesOutOfRange
        else Ok minutes

    let private validateMinutesIncrement increment =
        match commonIncrementValidation increment with
        | Some increment -> Ok increment
        | None -> Error MinutesIncrementOutOfRange

    let private validateHours hours =
        if (hours < 0 || hours > 23) then Error HoursOutOfRange
        else Ok hours

    let private validateHoursIncrement increment =
        match commonIncrementValidation increment with
        | Some increment -> Ok increment
        | None -> Error HoursIncrementOutOfRange

    let private validateDayOfMonth dayOfMonth =
        if (dayOfMonth < 0 || dayOfMonth > 31) then Error DayOfMonthOutOfRange
        else Ok dayOfMonth

    let private validateDayOfMonthIncrement increment =
        match commonIncrementValidation increment with
        | Some increment -> Ok increment
        | None -> Error DayOfMonthIncrementOutOfRange

    let private validateYear year =
        if (year < 1970 || year > 2099) then Error YearOutOfRange
        else Ok year

    let private validateYearIncrement increment =
        match commonIncrementValidation increment with
        | Some increment -> Ok increment
        | None -> Error YearIncrementOutOfRange

    let private validateDayOfWeekIncrement increment =
        match commonIncrementValidation increment with
        | Some increment -> Ok increment
        | None -> Error DayOfWeekIncrementOutOfRange

    let private validateMonthIncrement increment =
        match commonIncrementValidation increment with
        | Some increment -> Ok increment
        | None -> Error MonthIncrementOutOfRange

    let CreateDefaultCron() =
        { Seconds = "*"
          Minutes = "*"
          Hours = "*"
          DayOfMonth = "?"
          Month = "*"
          DayOfWeek = "*"
          Year = "*" }

    let SetSeconds secs cron =
        result {
            let! secs = validateSeconds secs
            return { cron with Seconds = (sprintf "%d" secs) } }

    let SetMinutes minutes cron =
        result {
            let! minutes = validateMinutes minutes
            return { cron with Minutes = (sprintf "%d" minutes) } }

    let SetHours hours cron =
        result {
            let! hours = validateHours hours
            return { cron with Hours = (sprintf "%d" hours) } }

    let SetDayOfMonth dayOfMonth cron =
        result {
            let! dayOfMonth = validateDayOfMonth dayOfMonth
            return { cron with
                         DayOfMonth = (sprintf "%d" dayOfMonth)
                         DayOfWeek = "?" }
        }

    let SetMonth month cron =
        result {
            let! month = getNumericMonth month
            return { cron with Month = (sprintf "%d" month) } }

    let SetDayOfWeek dayOfWeek cron =
        result {
            let! dayOfWeek = getNumericDay dayOfWeek
            return { cron with
                         DayOfWeek = (sprintf "%d" dayOfWeek)
                         DayOfMonth = "?" }
        }

    let SetYear year cron =
        result {
            let! year = validateYear year
            return { cron with Year = (sprintf "%d" year) } }

    let SetSecondsWithIncrement secs increment cron =
        result {
            let! secs = validateSeconds secs
            let! increment = validateSecondsIncrement increment
            return { cron with Seconds = (sprintf "%d/%d" secs increment) } }

    let SetMinutesWithIncrement minutes increment cron =
        result {
            let! minutes = validateMinutes minutes
            let! increment = validateMinutesIncrement increment
            return { cron with Minutes = (sprintf "%d/%d" minutes increment) } }

    let SetHoursWithIncrement hours increment cron =
        result {
            let! hours = validateHours hours
            let! increment = validateHoursIncrement increment
            return { cron with Hours = (sprintf "%d/%d" hours increment) } }

    let SetDayOfMonthWithIncrement dayOfMonth increment cron =
        result {
            let! dayOfMonth = validateDayOfMonth dayOfMonth
            let! increment = validateDayOfMonthIncrement increment
            return { cron with
                         DayOfMonth = (sprintf "%d/%d" dayOfMonth increment)
                         DayOfWeek = "?" }
        }

    let SetMonthWithIncrement month increment cron =
        result {
            let! month = getNumericMonth month
            let! increment = validateMonthIncrement increment
            return { cron with Month = (sprintf "%d/%d" month increment) } }

    let SetDayOfWeekWithIncrement dayOfWeek increment cron =
        result {
            let! dayOfWeek = getNumericDay dayOfWeek
            let! increment = validateDayOfWeekIncrement increment
            return { cron with
                         DayOfWeek = (sprintf "%d/%d" dayOfWeek increment)
                         DayOfMonth = "?" }
        }

    let SetYearWithIncrement year increment cron =
        result {
            let! year = validateYear year
            let! increment = validateYearIncrement increment
            return { cron with Year = (sprintf "%d/%d" year increment) } }

    let SetMultipleSeconds secs cron =
        result {
            let! secs = secs |> Helpers.TraverseListOfResultsM validateSeconds
            let secs = secs |> List.map (sprintf "%d")
            return { cron with Seconds = List.reduce (fun x y -> x + "," + y) secs }
        }

    let SetMultipleMinutes minutes cron =
        result {
            let! minutes = minutes |> Helpers.TraverseListOfResultsM validateMinutes
            let minutes = minutes |> List.map (sprintf "%d")
            return { cron with Minutes = List.reduce (fun x y -> x + "," + y) minutes }
        }

    let SetMultipleHours hours cron =
        result {
            let! hours = hours |> Helpers.TraverseListOfResultsM validateHours
            let hours = hours |> List.map (sprintf "%d")
            return { cron with Hours = List.reduce (fun x y -> x + "," + y) hours }
        }

    let SetMultipleDaysOfMonth daysOfMonth cron =
        result {
            let! dom = daysOfMonth |> Helpers.TraverseListOfResultsM validateDayOfMonth
            let dom = dom |> List.map (sprintf "%d")
            return { cron with
                         DayOfMonth = List.reduce (fun x y -> x + "," + y) dom
                         DayOfWeek = "?" }
        }

    let SetMultipleMonths months cron =
        result {
            let! months = months |> Helpers.TraverseListOfResultsM getNumericMonth
            let months = months |> List.map (sprintf "%d")
            return { cron with Month = List.reduce (fun x y -> x + "," + y) months }
        }

    let SetMultipleDaysOfWeek daysOfWeek cron =
        result {
            let! dow = daysOfWeek |> Helpers.TraverseListOfResultsM getNumericDay
            let dow = dow |> List.map (sprintf "%d")
            return { cron with
                         DayOfWeek = List.reduce (fun x y -> x + "," + y) dow
                         DayOfMonth = "?" }
        }

    let SetMultipleYears years cron =
        result {
            let! years = years |> Helpers.TraverseListOfResultsM validateYear
            let years = years |> List.map (sprintf "%d")
            return { cron with Year = List.reduce (fun x y -> x + "," + y) years }
        }

    let SetEverySecond cron = { cron with Seconds = "*" }
    let SetEveryMinute cron = { cron with Minutes = "*" }
    let SetEveryHour cron = { cron with Hours = "*" }

    let SetEveryDayOfMonth cron =
        { cron with
              DayOfMonth = "*"
              DayOfWeek = "?" }

    let SetEveryMonth cron = { cron with Month = "*" }

    let SetEveryDayOfWeek cron =
        { cron with
              DayOfWeek = "*"
              DayOfMonth = "?" }

    let SetEveryYear cron = { cron with Year = "*" }
    let SetNoSpecificSecond cron = { cron with Seconds = "?" }
    let SetNoSpecificMinute cron = { cron with Minutes = "?" }
    let SetNoSpecificHour cron = { cron with Hours = "?" }
    let SetNoSpecificDayOfMonth cron = { cron with DayOfMonth = "?" }
    let SetNoSpecificMonth cron = { cron with Month = "?" }
    let SetNoSpecificDayOfWeek cron = { cron with DayOfWeek = "?" }
    let SetNoSpecificYear cron = { cron with Year = "?" }

    let SetSecondsRange lower upper cron =
        if (lower < 0 || lower > 59) then Error SecondsOutOfRange
        else if (upper < 0 || upper > 59) then Error SecondsOutOfRange
        else if (lower >= upper) then Error SecondsOutOfRange
        else Ok { cron with Seconds = (sprintf "%d-%d" lower upper) }

    let SetMinutesRange lower upper cron =
        if (lower < 0 || lower > 59) then Error MinutesOutOfRange
        else if (upper < 0 || upper > 59) then Error MinutesOutOfRange
        else if (lower >= upper) then Error MinutesOutOfRange
        else Ok { cron with Minutes = (sprintf "%d-%d" lower upper) }

    let SetHoursRange lower upper cron =
        if (lower < 0 || lower > 23) then Error HoursOutOfRange
        else if (upper < 0 || upper > 23) then Error HoursOutOfRange
        else if (lower >= upper) then Error HoursOutOfRange
        else Ok { cron with Hours = (sprintf "%d-%d" lower upper) }

    let SetDayOfMonthRange lower upper cron =
        if (lower < 1 || lower > 31) then
            Error DayOfMonthOutOfRange
        else if (upper < 1 || upper > 31) then
            Error DayOfMonthOutOfRange
        else if (lower >= upper) then
            Error DayOfMonthOutOfRange
        else
            Ok
                { cron with
                      DayOfMonth = (sprintf "%d-%d" lower upper)
                      DayOfWeek = "?" }

    let SetMonthRange lower upper cron =
        result {
            let! lower = getNumericMonth lower
            let! upper = getNumericMonth upper
            if (lower < upper) then return { cron with Month = (sprintf "%d-%d" lower upper) }
            else return! Error MonthOutOfRange
        }

    let SetDayOfWeekRange lower upper cron =
        result {
            let! lower = getNumericDay lower
            let! upper = getNumericDay upper
            if (lower < upper) then
                return { cron with
                             DayOfWeek = (sprintf "%d-%d" lower upper)
                             DayOfMonth = "?" }
            else
                return! Error DayOfWeekOutOfRange
        }

    let SetYearRange lower upper cron =
        if (lower < 1970 || lower > 2099) then Error YearOutOfRange
        else if (upper < 1970 || upper > 2099) then Error YearOutOfRange
        else if (lower >= upper) then Error YearOutOfRange
        else Ok { cron with Year = (sprintf "%d-%d" lower upper) }

    let FromString cronString =
        if (Quartz.CronExpression.IsValidExpression cronString) then
            let fields = cronString.Split(' ')
            if (Array.length fields = 6) then
                Ok
                    { Seconds = fields.[0]
                      Minutes = fields.[1]
                      Hours = fields.[2]
                      DayOfMonth = fields.[3]
                      Month = fields.[4]
                      DayOfWeek = fields.[5]
                      Year = "" }
            else if (Array.length fields = 7) then
                Ok
                    { Seconds = fields.[0]
                      Minutes = fields.[1]
                      Hours = fields.[2]
                      DayOfMonth = fields.[3]
                      Month = fields.[4]
                      DayOfWeek = fields.[5]
                      Year = fields.[6] }
            else
                Error CronStringInvalid
        else
            Error CronStringInvalid

    let ToString cron =
        sprintf "%s %s %s %s %s %s %s" cron.Seconds cron.Minutes cron.Hours cron.DayOfMonth cron.Month cron.DayOfWeek
            cron.Year

    let ValidateCronString cronString = Quartz.CronExpression.IsValidExpression cronString
