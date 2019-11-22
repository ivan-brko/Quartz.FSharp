# Quartz.FSharp

## Basic info
This is an F# wrapper for [Quartz.net library](https://www.quartz-scheduler.net/). It allows using the library from a more F# friendly API.

## Supported Quartz.<span></span>net functionalities
At this moment, support is only added for unrecoverable jobs (jobs stored in RAM). This wrapper library is a result of needing to use Quartz.<span></span>net unrecoverable jobs in another project so no immediate support is planned for other types of serializations. However, other types of jobs/serializations can be added if needed.

## Example usage
Simple example can be found in Quartz.FSharp.Unrecoverable.Demo project.
Minimal version of that is following:

```fsharp
    let cron = Cron.CreateDefaultCron() |> Cron.SetEverySecond
    use context = new Context.QuartzSchedulingContext()

    let schedulingResult =
        UnrecoverableJob.ScheduleJobForCommonGroup context cron
            Context.TaskPriority.Medium "Example Task"
            (fun () -> printfn "Task Called")

    match schedulingResult with
    | Ok(_) -> Context.StartRunningScheduledTasks context        
    | Error(_) -> printfn "Error while trying to schedule task"
```

## Reporting issues
Issue report should contain basic information about the issue and instructions on how to reproduce it.

## Contributing
All contributions are welcome, both bug-fixes and improvements/new features.

## License
The project is under MIT license which means both comercial and non-comercial use is allowed.