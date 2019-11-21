namespace Quartz.Fsharp

module Configuration =
    open System.Collections.Specialized

    type ConfigurationCreationError =
        | ThreadCountTooHigh
        | ThreadCountTooLow

    type QuartzConfiguration = private QuartzConfiguration of NameValueCollection

    let Unwrap(QuartzConfiguration prop) = prop
    let CreateDefaultQuartzConfiguration() = QuartzConfiguration(NameValueCollection())

    let SetThreadCount (QuartzConfiguration properties) threadCount =
        if threadCount >= 1 && threadCount <= 20 then
            let newProperties = NameValueCollection(properties)
            newProperties.Set("quartz.threadPool.threadCount", threadCount.ToString())
            Ok(QuartzConfiguration newProperties)
        elif threadCount < 1 then
            Error ThreadCountTooLow
        else
            Error ThreadCountTooHigh

    let SetSchedulerInstanceName (QuartzConfiguration properties) schedulerInstanceName =
        let newProperties = NameValueCollection(properties)
        newProperties.Set("quartz.scheduler.instanceName", schedulerInstanceName)
        QuartzConfiguration newProperties

    let SetKeyValue (QuartzConfiguration properties) key value =
        let newProperties = NameValueCollection(properties)
        newProperties.Set(key, value)
        QuartzConfiguration newProperties
