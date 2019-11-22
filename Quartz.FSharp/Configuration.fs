namespace Quartz.Fsharp

module Configuration =
    open System.Collections.Specialized

    type ConfigurationCreationError =
        | ThreadCountTooHigh
        | ThreadCountTooLow

    type QuartzConfiguration = private QuartzConfiguration of NameValueCollection

    let Unwrap(QuartzConfiguration prop) = prop

    /// <summary>
    /// Creates a default Quartz.Net configuration (an empty <c>NameValueCollection</c>)
    /// </summary>
    /// <returns> A default Quartz.Net configuration wrapped in single case DU </returns>
    let CreateDefaultQuartzConfiguration() = QuartzConfiguration(NameValueCollection())


    /// <summary>
    /// Sets a thread count for Quartz configuration.
    /// Does not modify the properties it receives, but creates new properties.
    /// </summary>
    /// <param name="threadCount">Wanted thread count</param>
    /// <param name="properties">Quartz configuration whose fields will be copied in the result configuration</param>
    /// <returns> If threads are in allowed range (between 1 and 20), returns Ok with new configuration, otherwise returns an Error</returns>
    let SetThreadCount threadCount (QuartzConfiguration properties) =
        if threadCount >= 1 && threadCount <= 20 then
            let newProperties = NameValueCollection(properties)
            newProperties.Set("quartz.threadPool.threadCount", threadCount.ToString())
            Ok(QuartzConfiguration newProperties)
        elif threadCount < 1 then
            Error ThreadCountTooLow
        else
            Error ThreadCountTooHigh

    /// <summary>
    /// Sets the name for scheduler instance. Refer to Quartz.Net documentation for more information about instance names
    /// </summary>
    /// <param name="schedulerInstanceName">Wanted instance name</param>
    /// <param name="properties">Quartz configuration whose fields will be copied in the result configuration</param>
    /// <returns> New configuration with instance name set to wanted value</returns>
    let SetSchedulerInstanceName schedulerInstanceName (QuartzConfiguration properties) =
        let newProperties = NameValueCollection(properties)
        newProperties.Set("quartz.scheduler.instanceName", schedulerInstanceName)
        QuartzConfiguration newProperties

    let SetKeyValue (QuartzConfiguration properties) key value =
        let newProperties = NameValueCollection(properties)
        newProperties.Set(key, value)
        QuartzConfiguration newProperties
