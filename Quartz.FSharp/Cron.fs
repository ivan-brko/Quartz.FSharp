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

    /// <summary>
    /// Creates a default cron that defines running every second
    /// </summary>
    /// <returns> Default cron</returns>
    let CreateDefaultCron() =
        { Seconds = "*"
          Minutes = "*"
          Hours = "*"
          DayOfMonth = "?"
          Month = "*"
          DayOfWeek = "*"
          Year = "*" }

    /// <summary>
    /// Set seconds in the given cron and returns a new one with changed seconds
    /// </summary>
    /// <param name="secs"> Seconds parameter to be set in the cron</param>
    /// <param name="cron"> Cron whose seconds need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if secods are in valid range, Error othervise</returns>
    let SetSeconds secs cron =
        result {
            let! secs = validateSeconds secs
            return { cron with Seconds = (sprintf "%d" secs) } }

    /// <summary>
    /// Set minutes in the given cron and returns a new one with changed minutes
    /// </summary>
    /// <param name="minutes"> Minutes parameter to be set in the cron</param>
    /// <param name="cron"> Cron whose minutes need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if minutes are in valid range, Error othervise</returns>
    let SetMinutes minutes cron =
        result {
            let! minutes = validateMinutes minutes
            return { cron with Minutes = (sprintf "%d" minutes) } }

    /// <summary>
    /// Set hours in the given cron and returns a new one with changed hours
    /// </summary>
    /// <param name="hours"> hours parameter to be set in the cron</param>
    /// <param name="cron"> Cron whose hours need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if hours are in valid range, Error othervise</returns>
    let SetHours hours cron =
        result {
            let! hours = validateHours hours
            return { cron with Hours = (sprintf "%d" hours) } }

    /// <summary>
    /// Set day of month in the given cron and returns a new one with changed day of month
    /// </summary>
    /// <param name="dayOfMonth"> Day of month parameter to be set in the cron</param>
    /// <param name="cron"> Cron whose day of month need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if day of month are in valid range, Error othervise</returns>
    let SetDayOfMonth dayOfMonth cron =
        result {
            let! dayOfMonth = validateDayOfMonth dayOfMonth
            return { cron with
                         DayOfMonth = (sprintf "%d" dayOfMonth)
                         DayOfWeek = "?" }
        }

    /// <summary>
    /// Set month in the given cron and returns a new one with changed month
    /// </summary>
    /// <param name="month"> Month parameter to be set in the cron</param>
    /// <param name="cron"> Cron whose month needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if month is in valid range, Error othervise</returns>
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

    /// <summary>
    /// Set year in the given cron and returns a new one with changed year
    /// </summary>
    /// <param name="year"> Year parameter to be set in the cron</param>
    /// <param name="cron"> Cron whose year needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if year is in valid range, Error othervise</returns>
    let SetYear year cron =
        result {
            let! year = validateYear year
            return { cron with Year = (sprintf "%d" year) } }

    /// <summary>
    /// Set seconds with increment in the given cron and returns a new one with changed seconds
    /// For more information about increment in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="secs"> Seconds parameter to be set in the cron</param>
    /// <param name="increment"> Increment parameter to be set in the cron</param>
    /// <param name="cron"> Cron whose seconds need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if seconds and increment are in valid range, Error othervise</returns>
    let SetSecondsWithIncrement secs increment cron =
        result {
            let! secs = validateSeconds secs
            let! increment = validateSecondsIncrement increment
            return { cron with Seconds = (sprintf "%d/%d" secs increment) } }

    /// <summary>
    /// Set mintues with increment in the given cron and returns a new one with changed minutes
    /// For more information about increment in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="minutes"> Minutes parameter to be set in the cron</param>
    /// <param name="increment"> Increment parameter to be set in the cron</param>
    /// <param name="cron"> Cron whose minutes need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if minutes and increment are in valid range, Error othervise</returns>
    let SetMinutesWithIncrement minutes increment cron =
        result {
            let! minutes = validateMinutes minutes
            let! increment = validateMinutesIncrement increment
            return { cron with Minutes = (sprintf "%d/%d" minutes increment) } }

    /// <summary>
    /// Set hours with increment in the given cron and returns a new one with changed hours
    /// For more information about increment in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="hours"> Hours parameter to be set in the cron</param>
    /// <param name="increment"> Increment parameter to be set in the cron</param>
    /// <param name="cron"> Cron whose hours need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if hours and increment are in valid range, Error othervise</returns>
    let SetHoursWithIncrement hours increment cron =
        result {
            let! hours = validateHours hours
            let! increment = validateHoursIncrement increment
            return { cron with Hours = (sprintf "%d/%d" hours increment) } }

    /// <summary>
    /// Set day of month with increment in the given cron and returns a new one with changed day of month
    /// For more information about increment in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="dayOfMonth"> Day of month parameter to be set in the cron</param>
    /// <param name="increment"> Increment parameter to be set in the cron</param>
    /// <param name="cron"> Cron whose day of month needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if day of month and increment are in valid range, Error othervise</returns>
    let SetDayOfMonthWithIncrement dayOfMonth increment cron =
        result {
            let! dayOfMonth = validateDayOfMonth dayOfMonth
            let! increment = validateDayOfMonthIncrement increment
            return { cron with
                         DayOfMonth = (sprintf "%d/%d" dayOfMonth increment)
                         DayOfWeek = "?" }
        }

    /// <summary>
    /// Set month with increment in the given cron and returns a new one with changed month
    /// For more information about increment in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="month"> Month parameter to be set in the cron</param>
    /// <param name="increment"> Increment parameter to be set in the cron</param>
    /// <param name="cron"> Cron whose month needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if month and increment are in valid range, Error othervise</returns>
    let SetMonthWithIncrement month increment cron =
        result {
            let! month = getNumericMonth month
            let! increment = validateMonthIncrement increment
            return { cron with Month = (sprintf "%d/%d" month increment) } }

    /// <summary>
    /// Set day of week with increment in the given cron and returns a new one with changed day of week
    /// For more information about increment in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="dayOfWeek"> Day of week parameter to be set in the cron</param>
    /// <param name="increment"> Increment parameter to be set in the cron</param>
    /// <param name="cron"> Cron whose day of week need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if day of week and increment are in valid range, Error othervise</returns>
    let SetDayOfWeekWithIncrement dayOfWeek increment cron =
        result {
            let! dayOfWeek = getNumericDay dayOfWeek
            let! increment = validateDayOfWeekIncrement increment
            return { cron with
                         DayOfWeek = (sprintf "%d/%d" dayOfWeek increment)
                         DayOfMonth = "?" }
        }

    /// <summary>
    /// Set year with increment in the given cron and returns a new one with changed year
    /// For more information about increment in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="year"> Year parameter to be set in the cron</param>
    /// <param name="increment"> Increment parameter to be set in the cron</param>
    /// <param name="cron"> Cron whose years need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if year and increment are in valid range, Error othervise</returns>
    let SetYearWithIncrement year increment cron =
        result {
            let! year = validateYear year
            let! increment = validateYearIncrement increment
            return { cron with Year = (sprintf "%d/%d" year increment) } }

    /// <summary>
    /// Set multiple seconds in the given cron and returns a new one with changed seconds
    /// For more information about multiple values in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="secs"> List containing all seconds to be set in the cron</param>
    /// <param name="cron"> Cron whose seconds need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if all seconds are in valid range, Error othervise</returns>
    let SetMultipleSeconds secs cron =
        result {
            let! secs = secs |> Helpers.traverseListOfResultsM validateSeconds
            let secs = secs |> List.map (sprintf "%d")
            return { cron with Seconds = List.reduce (fun x y -> x + "," + y) secs }
        }

    /// <summary>
    /// Set multiple minutes in the given cron and returns a new one with changed minutes
    /// For more information about multiple values in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="minutes"> List containing all minutes to be set in the cron</param>
    /// <param name="cron"> Cron whose minutes need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if all minutes are in valid range, Error othervise</returns>
    let SetMultipleMinutes minutes cron =
        result {
            let! minutes = minutes |> Helpers.traverseListOfResultsM validateMinutes
            let minutes = minutes |> List.map (sprintf "%d")
            return { cron with Minutes = List.reduce (fun x y -> x + "," + y) minutes }
        }

    /// <summary>
    /// Set multiple hours in the given cron and returns a new one with changed hours
    /// For more information about multiple values in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="hours"> List containing all hours to be set in the cron</param>
    /// <param name="cron"> Cron whose hours need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if all hours are in valid range, Error othervise</returns>
    let SetMultipleHours hours cron =
        result {
            let! hours = hours |> Helpers.traverseListOfResultsM validateHours
            let hours = hours |> List.map (sprintf "%d")
            return { cron with Hours = List.reduce (fun x y -> x + "," + y) hours }
        }

    /// <summary>
    /// Set multiple days of month in the given cron and returns a new one with changed days of month
    /// For more information about multiple values in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="daysOfMonth"> List containing all days of month to be set in the cron</param>
    /// <param name="cron"> Cron whose days of month need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if all days of month are in valid range, Error othervise</returns>
    let SetMultipleDaysOfMonth daysOfMonth cron =
        result {
            let! dom = daysOfMonth |> Helpers.traverseListOfResultsM validateDayOfMonth
            let dom = dom |> List.map (sprintf "%d")
            return { cron with
                         DayOfMonth = List.reduce (fun x y -> x + "," + y) dom
                         DayOfWeek = "?" }
        }

    /// <summary>
    /// Set multiple months in the given cron and returns a new one with changed months
    /// For more information about multiple values in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="months"> List containing all months to be set in the cron</param>
    /// <param name="cron"> Cron whose months need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if all months are in valid range, Error othervise</returns>
    let SetMultipleMonths months cron =
        result {
            let! months = months |> Helpers.traverseListOfResultsM getNumericMonth
            let months = months |> List.map (sprintf "%d")
            return { cron with Month = List.reduce (fun x y -> x + "," + y) months }
        }

    /// <summary>
    /// Set multiple days of week the given cron and returns a new one with changed days of week
    /// For more information about multiple values in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="daysOfWeek"> List containing all days of week to be set in the cron</param>
    /// <param name="cron"> Cron whose days of week need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if all days of week are in valid range, Error othervise</returns>
    let SetMultipleDaysOfWeek daysOfWeek cron =
        result {
            let! dow = daysOfWeek |> Helpers.traverseListOfResultsM getNumericDay
            let dow = dow |> List.map (sprintf "%d")
            return { cron with
                         DayOfWeek = List.reduce (fun x y -> x + "," + y) dow
                         DayOfMonth = "?" }
        }

    /// <summary>
    /// Set multiple years in the given cron and returns a new one with changed years
    /// For more information about multiple values in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="years"> List containing all years to be set in the cron</param>
    /// <param name="cron"> Cron whose years need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if all years are in valid range, Error othervise</returns>
    let SetMultipleYears years cron =
        result {
            let! years = years |> Helpers.traverseListOfResultsM validateYear
            let years = years |> List.map (sprintf "%d")
            return { cron with Year = List.reduce (fun x y -> x + "," + y) years }
        }

    /// <summary>
    /// Set every second in the given cron and returns a new one with changed seconds
    /// </summary>
    /// <param name="cron"> Cron whose seconds need to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated seconds</returns>
    let SetEverySecond cron = { cron with Seconds = "*" }

    /// <summary>
    /// Set every minute in the given cron and returns a new one with changed minutes
    /// </summary>
    /// <param name="cron"> Cron whose minutes need to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated minutes</returns>
    let SetEveryMinute cron = { cron with Minutes = "*" }

    /// <summary>
    /// Set every hour in the given cron and returns a new one with changed hour
    /// </summary>
    /// <param name="cron"> Cron whose hour need to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated hours</returns>
    let SetEveryHour cron = { cron with Hours = "*" }

    /// <summary>
    /// Set every day of the month in the given cron and returns a new one with changed day of month and day of week
    /// Day of week needs to be set to <c>?</c> after changing day of month
    /// </summary>
    /// <param name="cron"> Cron whose day of month needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated day of month and day of week</returns>
    let SetEveryDayOfMonth cron =
        { cron with
              DayOfMonth = "*"
              DayOfWeek = "?" }

    /// <summary>
    /// Set every month in the given cron and returns a new one with changed month
    /// </summary>
    /// <param name="cron"> Cron whose month needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated month</returns>
    let SetEveryMonth cron = { cron with Month = "*" }

    /// <summary>
    /// Set every day of the week in the given cron and returns a new one with changed day of week and day of month
    /// Day of month needs to be set to <c>?</c> after changing day of week
    /// </summary>
    /// <param name="cron"> Cron whose day of week needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated day of week and day of month</returns>
    let SetEveryDayOfWeek cron =
        { cron with
              DayOfWeek = "*"
              DayOfMonth = "?" }

    /// <summary>
    /// Set every year in the given cron and returns a new one with changed year
    /// </summary>
    /// <param name="cron"> Cron whose year needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated year</returns>
    let SetEveryYear cron = { cron with Year = "*" }

    /// <summary>
    /// Set no specific secons in the given cron and returns a new one with changed seconds
    /// </summary>
    /// <param name="cron"> Cron whose seconds need to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated seconds</returns>
    let SetNoSpecificSecond cron = { cron with Seconds = "?" }

    /// <summary>
    /// Set no specific minutes in the given cron and returns a new one with changed minutes
    /// </summary>
    /// <param name="cron"> Cron whose minutes need to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated minutes</returns>
    let SetNoSpecificMinute cron = { cron with Minutes = "?" }

    /// <summary>
    /// Set no specific hour in the given cron and returns a new one with changed hour
    /// </summary>
    /// <param name="cron"> Cron whose hour needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated hour</returns>
    let SetNoSpecificHour cron = { cron with Hours = "?" }

    /// <summary>
    /// Set no specific day of month in the given cron and returns a new one with changed day of month
    /// </summary>
    /// <param name="cron"> Cron whose day of month needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated day of month</returns>
    let SetNoSpecificDayOfMonth cron = { cron with DayOfMonth = "?" }

    /// <summary>
    /// Set no specific month in the given cron and returns a new one with changed month
    /// </summary>
    /// <param name="cron"> Cron whose month needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated month</returns>
    let SetNoSpecificMonth cron = { cron with Month = "?" }

    /// <summary>
    /// Set no specific day of week in the given cron and returns a new one with changed day of week
    /// </summary>
    /// <param name="cron"> Cron whose day of week needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated day of week</returns>
    let SetNoSpecificDayOfWeek cron = { cron with DayOfWeek = "?" }

    /// <summary>
    /// Set no specific year in the given cron and returns a new one with changed year
    /// </summary>
    /// <param name="cron"> Cron whose year needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> New cron with updated year</returns>
    let SetNoSpecificYear cron = { cron with Year = "?" }

    /// <summary>
    /// Set range for seconds in the given cron and returns a new one with changed seconds
    /// For more information about ranges in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="lower"> Lower value for seconds</param>
    /// <param name="uppper"> Upper value for seconds</param>
    /// <param name="cron"> Cron whose seconds need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if both upper and lower are in valid range, Error othervise</returns>
    let SetSecondsRange lower upper cron =
        if (lower < 0 || lower > 59) then Error SecondsOutOfRange
        else if (upper < 0 || upper > 59) then Error SecondsOutOfRange
        else if (lower >= upper) then Error SecondsOutOfRange
        else Ok { cron with Seconds = (sprintf "%d-%d" lower upper) }

    /// <summary>
    /// Set range for minutes in the given cron and returns a new one with changed minutes
    /// For more information about ranges in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="lower"> Lower value for minutes</param>
    /// <param name="uppper"> Upper value for minutes</param>
    /// <param name="cron"> Cron whose minutes need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if both upper and lower are in valid range, Error othervise</returns>
    let SetMinutesRange lower upper cron =
        if (lower < 0 || lower > 59) then Error MinutesOutOfRange
        else if (upper < 0 || upper > 59) then Error MinutesOutOfRange
        else if (lower >= upper) then Error MinutesOutOfRange
        else Ok { cron with Minutes = (sprintf "%d-%d" lower upper) }

    /// <summary>
    /// Set range for hours in the given cron and returns a new one with changed hours
    /// For more information about ranges in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="lower"> Lower value for hours</param>
    /// <param name="uppper"> Upper value for hours</param>
    /// <param name="cron"> Cron whose hours need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if both upper and lower are in valid range, Error othervise</returns>
    let SetHoursRange lower upper cron =
        if (lower < 0 || lower > 23) then Error HoursOutOfRange
        else if (upper < 0 || upper > 23) then Error HoursOutOfRange
        else if (lower >= upper) then Error HoursOutOfRange
        else Ok { cron with Hours = (sprintf "%d-%d" lower upper) }

    /// <summary>
    /// Set range for day of month in the given cron and returns a new one with changed day of month
    /// For more information about ranges in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="lower"> Lower value for day of month</param>
    /// <param name="uppper"> Upper value for day of month</param>
    /// <param name="cron"> Cron whose day of month needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if both upper and lower are in valid range, Error othervise</returns>
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

    /// <summary>
    /// Set range for month in the given cron and returns a new one with changed month
    /// For more information about ranges in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="lower"> Lower value for month</param>
    /// <param name="uppper"> Upper value for month</param>
    /// <param name="cron"> Cron whose month needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if both upper and lower are in valid range, Error othervise</returns>
    let SetMonthRange lower upper cron =
        result {
            let! lower = getNumericMonth lower
            let! upper = getNumericMonth upper
            if (lower < upper) then return { cron with Month = (sprintf "%d-%d" lower upper) }
            else return! Error MonthOutOfRange
        }

    /// <summary>
    /// Set range for day of week in the given cron and returns a new one with changed day of week
    /// For more information about ranges in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="lower"> Lower value for day of week</param>
    /// <param name="uppper"> Upper value for day of week</param>
    /// <param name="cron"> Cron whose day of week needs to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if both upper and lower are in valid range, Error othervise</returns>
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

    /// <summary>
    /// Set range for years in the given cron and returns a new one with changed years
    /// For more information about ranges in quartz.net read the following: https://www.quartz-scheduler.net/documentation/quartz-3.x/tutorial/crontrigger.html
    /// </summary>
    /// <param name="lower"> Lower value for years</param>
    /// <param name="uppper"> Upper value for years</param>
    /// <param name="cron"> Cron whose years need to be updated (new Cron object will be created with the result)</param>
    /// <returns> A Result containing new cron if both upper and lower are in valid range, Error othervise</returns>
    let SetYearRange lower upper cron =
        if (lower < 1970 || lower > 2099) then Error YearOutOfRange
        else if (upper < 1970 || upper > 2099) then Error YearOutOfRange
        else if (lower >= upper) then Error YearOutOfRange
        else Ok { cron with Year = (sprintf "%d-%d" lower upper) }

    /// <summary>
    /// Tries to create a cron record from string
    /// </summary>
    /// <param name="cronString"> String containing a cron value</param>
    /// <returns> A Result containing cron record if <c>cronString</c> is valid cron, Error othervise</returns>
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

    /// <summary>
    /// Generates a string from cron record
    /// </summary>
    /// <param name="cron"> A cron record from which the string will be created</param>
    /// <returns> A string generated from the cron record</returns>
    let ToString cron =
        sprintf "%s %s %s %s %s %s %s" cron.Seconds cron.Minutes cron.Hours cron.DayOfMonth cron.Month cron.DayOfWeek
            cron.Year

    /// <summary>
    /// Checks if given string is valid cron (in context of Quartz.Net)
    /// </summary>
    /// <param name="cron"> A cron record to check</param>
    /// <returns> True if given string is valid cron value, false otherwise</returns>
    let ValidateCronString cronString = Quartz.CronExpression.IsValidExpression cronString
