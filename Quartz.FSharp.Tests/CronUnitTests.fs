module ``Unit tests for Cron module``

    open NUnit.Framework
    open FsUnit
    open Quartz.Fsharp

    [<Test>]
    let ``Creating a default cron`` () = 
        let cron = Cron.CreateDefaultCron()

        cron.Seconds
        |> should equal "*"

        cron.Minutes
        |> should equal "*"

        cron.Hours
        |> should equal "*"

        cron.DayOfMonth
        |> should equal "?"

        cron.Month
        |> should equal "*"

        cron.DayOfWeek
        |> should equal "*"

        cron.Year
        |> should equal "*"
        
        cron |> Cron.ToString
        |> should equal "* * * ? * * *"
        
        cron |> Cron.ToString
        |> Cron.ValidateCronString
        |> should be True

