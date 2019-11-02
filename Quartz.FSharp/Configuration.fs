namespace Quartz.Fsharp

module Configuration =
    open System.Collections.Specialized

    type ConfigurationCreationError =
        | ThreadCountTooHigh
        | ThreadCountTooLow

    type QuartzConfiguration = private QuartzConfiguration of NameValueCollection

    let Unwrap(QuartzConfiguration prop) = prop
    let CreateDefaultQuartzConfiguration() = QuartzConfiguration(NameValueCollection())

    //todo: add more functions like this for commonly used properties
    let ChangeThreadCountInConfiguration threadCount (QuartzConfiguration properties) =
        if threadCount >= 1 && threadCount <= 20 then //TODO: these numbers are random, think about this later
            //this will create new NVCollection and copy elements from the initial one to it
            //these collections will usually be empty here, or only contain a couple of elements and this
            //will only get called at initialization so it the overhead is not too big, and this allows
            //us a more F# like API with chaining calls
            let newProperties = NameValueCollection(properties)
            newProperties.Set("quartz.threadPool.threadCount", threadCount.ToString())
            Ok(QuartzConfiguration newProperties)
        elif threadCount < 1 then Error ThreadCountTooLow
        else Error ThreadCountTooHigh

    //todo: in the future, remove this function and add safe setters for all properties
    let ChangeInProperties key value (QuartzConfiguration properties) =
        let newProperties = NameValueCollection(properties)
        newProperties.Set(key, value)
        QuartzConfiguration newProperties
