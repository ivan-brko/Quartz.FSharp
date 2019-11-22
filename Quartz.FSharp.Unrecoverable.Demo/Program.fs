open System
open Quartz.Fsharp

let getTimestamp() = DateTime.Now.ToString("HH:mm:ss")

[<EntryPoint>]
let main argv =
    printfn "Running a demo of Quartz.FSharp unrecoverable job"

    // create a cron that will run the task every second
    let cron = Cron.CreateDefaultCron() |> Cron.SetEverySecond

    //create a default configuration and set the thread count to 5
    let configuration = Configuration.CreateDefaultQuartzConfiguration()
                            |> Configuration.SetThreadCount 5

    // create new context
    // use created configuration if it is valid, if not use no configuration
    use context = match configuration with 
                  | Ok configuration -> new Context.QuartzSchedulingContext(configuration)
                  | Error _ ->  new Context.QuartzSchedulingContext()
    
    // the logging function
    // level gives the level of logging message (DEBUG, INFO...), funOpt is optional that, 
    // if set to Some, will contain a function that will generate the message. 
    // Last two args (not used here) are exception and params. 
    // Exception will contain the exception if it has occured so that it can be logged 
    // and params carry some additional data. Refer to LibLog docs for more information
    let loggerFunction level funOpt _ _ = 
        match funOpt with
        | Some f -> printfn "Logger: %A: %s" level (f())
                    true
        | None -> true            

    // Use the logging function created above
    Logging.SetQuartzLoggingFunction loggerFunction    

    // Schedule the task
    let schedulingResult =
        UnrecoverableJob.ScheduleJobForCommonGroup context cron
            Context.TaskPriority.Medium "Example Task"
            (fun () -> printfn "%s: Task Called" (getTimestamp()))

    // If scheduling was OK, start running the task
    match schedulingResult with
    | Ok(_) -> Context.StartRunningScheduledTasks context        
    | Error(_) -> printfn "Error while trying to schedule task"

    // Wait for <enter> to complete the program
    let line = Console.ReadLine() |> ignore
    0 // return an integer exit code


//the output should contain lines like this, where lines starting with timestamp are the result of task being called
// and other lines are reported by the logger

// 17:41:04: Task Called
// Logger: Debug: Trigger instruction : NoInstruction
// Logger: Debug: Producing instance of Job 'groupAll.Example Task', class=Quartz.Fsharp.UnrecoverableJob+WrapperJob
// Logger: Debug: Calling Execute on job groupAll.Example Task
// Logger: Debug: Batch acquisition of {0} triggers
// 17:41:05: Task Called
// Logger: Debug: Trigger instruction : NoInstruction
// Logger: Debug: Producing instance of Job 'groupAll.Example Task', class=Quartz.Fsharp.UnrecoverableJob+WrapperJob
// Logger: Debug: Batch acquisition of {0} triggersLogger: Debug: 
// Calling Execute on job groupAll.Example Task
// 17:41:06: Task Called
// Logger: Debug: Trigger instruction : NoInstruction
// Logger: Debug: Producing instance of Job 'groupAll.Example Task', class=Quartz.Fsharp.UnrecoverableJob+WrapperJob
// Logger: Debug: Calling Execute on job groupAll.Example TaskLogger: Debug: Batch acquisition of {0} triggers

