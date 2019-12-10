![logo](project_metadata/quartz.fsharp.medium.png)

# Quartz.FSharp

## Basic info
This is an F# wrapper for [Quartz.net library](https://www.quartz-scheduler.net/). It allows using the library from a more F# friendly API. This wrapper is not written or maintained by the Quartz.<span></span>net team or in any way connected to them. 

## Getting the library
This library is available as [nuget package](https://www.nuget.org/packages/Quartz.FSharp/) (can be used by both nuget and paket). 

## Supported Quartz.<span></span>net functionalities
At this moment, support is only added for unrecoverable jobs (jobs stored in RAM). This wrapper library is a result of needing to use Quartz.<span></span>net unrecoverable jobs in another project so no immediate support is planned for other types of serializations. However, other types of jobs/serializations can be added if needed.

## Example usage
Commented simple example can be found in Quartz.FSharp.Unrecoverable.Demo project.
Following is minimal version of that example:

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
Issue reports should contain basic information about the issue and instructions on how to reproduce it, as well as version of the library for which the issue is reported.

## Contributing
All contributions are welcome, both bug-fixes and improvements/new features.

## License
The project is under MIT license which means both comercial and non-comercial use is allowed.