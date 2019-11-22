namespace Quartz.Fsharp

open Configuration
open Quartz.Impl
open System
open System.Reflection

//todo: ADD API TRIGGER SUPPORT

module Context =
    //this function uses reflection to get private configuration of the schedulerFactory. this
    //is needed if the user of the library uses configuration file instead of our api for setting
    //configuration as we have uses cases where we want to know if JobStore is in RAM or not
    //(so we need to read the underlying configuration to see that)
    let private getSchedulerFactoryPropertiesReader (schedulerFactory: StdSchedulerFactory) =
        //this function checks the private fields of the schedulerFactory, and returns PropertiesParser if and
        //only if there is one private field that has this type
        let getPropertyIfNotNamedCfg() =
            let privateFields = schedulerFactory.GetType().GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance)

            let configurationField =
                privateFields
                |> Seq.filter (fun x -> x.FieldType = typeof<Quartz.Util.PropertiesParser>)
                |> Seq.toList
            if configurationField.Length = 1 then
                Some(configurationField.[0].GetValue(schedulerFactory) :?> Quartz.Util.PropertiesParser)
            else None

        //first try getting the field called cfg (that is the name of this field ATM of writing this)
        let cfgField = schedulerFactory.GetType().GetField("cfg", BindingFlags.NonPublic ||| BindingFlags.Instance)
        match cfgField with
        | null -> getPropertyIfNotNamedCfg()
        | field ->
            if field.FieldType = typeof<Quartz.Util.PropertiesParser> then  //if this property is of right type
                Some(field.GetValue(schedulerFactory) :?> Quartz.Util.PropertiesParser)
            else getPropertyIfNotNamedCfg()

    type TaskPriority =
        | Low
        | Medium
        | High

    //note: default prio in quartz is 5
    //we never create with defult value, so it is not that important
    let getNumericValueForTaskPrio prio =
        match prio with
        | Low -> 1
        | Medium -> 10
        | High -> 100

    /// <summary>
    /// This class holds the Quartz scheduling context, used for scheduling all job types
    /// </summary>
    /// <param name="properties">Configuration to be used with this scheduling context</param>
    type QuartzSchedulingContext(properties: QuartzConfiguration) =
        let factory = StdSchedulerFactory(Unwrap properties)

        /// <summary>
        /// Empty constructor for <c>QuartzSchedulingContext</c>. Creates the context with default params.
        /// </summary>
        new() = new QuartzSchedulingContext(Configuration.CreateDefaultQuartzConfiguration())

        //check underlying properties of schedulerFacotry to determine configuration
        member this.IsStoredInRam =
            match (getSchedulerFactoryPropertiesReader factory) with
            | Some propertiesReader ->
                let serializationType =
                    propertiesReader.GetStringProperty("quartz.jobStore.type", "Quartz.Simpl.RAMJobStore, Quartz")
                serializationType = "Quartz.Simpl.RAMJobStore, Quartz"
            | None -> false //TODO: think about this, should this be more permissive?


        member this.Scheduler = async {
                                    let! scheduler = factory.GetScheduler() |> Async.AwaitTask
                                    return scheduler } |> Async.RunSynchronously
        member this.StartRunningScheduledTasks() =
            async { do! this.Scheduler.Start() |> Async.AwaitTask } |> Async.RunSynchronously
        member this.PauseAllJobs() =
            async { do! this.Scheduler.PauseAll() |> Async.AwaitTask } |> Async.RunSynchronously

        interface IDisposable with
            member this.Dispose() =
                async { do! this.Scheduler.Shutdown() |> Async.AwaitTask } |> Async.RunSynchronously

    //we allow using the context both as .net object and through a more idiomatic f# style by chainging functions


    /// <summary>
    /// Start running all jobs scheduled with this context
    /// </summary>
    /// <param name="context">Context for which we want to run all jobs</param>
    let StartRunningScheduledTasks(context: QuartzSchedulingContext) = context.StartRunningScheduledTasks()

    /// <summary>
    /// Pause running all jobs scheduled with this context
    /// </summary>
    /// <param name="context">Context for which we want to pause all jobs</param>
    let PauseAllJobs(context: QuartzSchedulingContext) = context.PauseAllJobs()
