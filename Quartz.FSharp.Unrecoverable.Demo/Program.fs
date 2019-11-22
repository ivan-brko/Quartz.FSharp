open System
open Quartz.Fsharp

let getTimestamp() = DateTime.Now.ToString("HH:mm:ss")

[<EntryPoint>]
let main argv =
    printfn "Running a demo of Quartz.FSharp unrecoverable job"
    let cron = Cron.CreateDefaultCron() |> Cron.SetEverySecond
    use context = new Context.QuartzSchedulingContext()
    
    let loggerFunction level funOpt _ _ = 
        match funOpt with
        | Some f -> printfn "Logger: %A: %s" level (f())
                    true
        | None -> true            

    Logging.SetQuartzLoggingFunction loggerFunction    

    let schedulingResult =
        UnrecoverableJob.ScheduleJobForCommonGroup context cron
            Context.TaskPriority.Medium "Example Task"
            (fun () -> printfn "%s: Task Called" (getTimestamp()))

    match schedulingResult with
    | Ok(_) -> Context.StartRunningScheduledTasks context        
    | Error(_) -> printfn "Error while trying to schedule task"

    let line = Console.ReadLine() |> ignore
    0 // return an integer exit code
