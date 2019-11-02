namespace Quartz.Fsharp
open Context
open Quartz.Fsharp.Cron
open Quartz
open System

module UnrecoverableJob =
    type JobStartingError = 
        | RamJobNotAllowedInNonRamContext

    type private WrapperJob() =
        interface IJob with
            // some ugly glue required to convert c# async api to f# async api (and vice-versa)
            member this.Execute(context : IJobExecutionContext) =
                async {
                    try
                        let dataMap = context.JobDetail.JobDataMap
                        let task = dataMap.["task"] :?> unit -> unit
                        task()
                    with ex -> raise (JobExecutionException ex.Message) //Execute method should only throw this type of exception
                }
                |> Async.StartAsTask :> System.Threading.Tasks.Task

    let private addTaskToScheduler (context : QuartzSchedulingContext) taskName taskCron prio (task : unit -> unit) groupName =
        let mutable jobDataMap = JobDataMap()
        jobDataMap.["task"] <- task
        let job =
            JobBuilder.Create<WrapperJob>().UsingJobData(jobDataMap)
                .WithIdentity(taskName, groupName)
                .Build()
        let trigger =
            TriggerBuilder.Create().WithIdentity((sprintf "%s-trig" taskName), (sprintf "%s-trig" groupName))
                          .WithCronSchedule(taskCron, fun x -> x.InTimeZone(TimeZoneInfo.Local) |> ignore)
                          .WithPriority(getNumericValueForTaskPrio prio).Build()
        async { let! offset = context.Scheduler.ScheduleJob(job, trigger) |> Async.AwaitTask
                return offset }
        |> Async.RunSynchronously
        |> ignore

    let ScheduleJobForCommonGroup (context : QuartzSchedulingContext) cron prio taskName task =
        if context.IsStoredInRam then
            let cronString = cron |> ToString
            addTaskToScheduler context taskName cronString prio task "groupAll"
            Ok ()
        else
            Error RamJobNotAllowedInNonRamContext

    let ScheduleJobForSpecificGroup (context : QuartzSchedulingContext) groupName cron prio taskName task =
        if context.IsStoredInRam then
            let cronString = cron |> ToString
            addTaskToScheduler context taskName cronString prio task groupName
            Ok()
        else 
            Error RamJobNotAllowedInNonRamContext

