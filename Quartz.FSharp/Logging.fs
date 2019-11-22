namespace Quartz.Fsharp

module Logging =
    open Quartz.Logging
    open System

    //todo: it seems that quartz doesn't use mapped and nested context,
    //however, check if this is the best implementation for this interface
    type private QuartzLoggerWrapper(f) =
        interface ILogProvider with

            member this.OpenMappedContext(_, _) =
                { new IDisposable with
                    member this.Dispose() = () }

            member this.OpenNestedContext _ =
                { new IDisposable with
                    member this.Dispose() = () }

            member this.GetLogger _name = new Logger(f)

    /// <summary>
    /// Sets a logging function.
    /// This function is just an f# wrapper around LibLog logging interface.
    /// The F# wrapper never calls this function, only the Quartz.Net does.
    /// </summary>
    /// <param name="f"> Function to do all the logging</param>
    let SetQuartzLoggingFunction f =
        let loggerFunction level (func: Func<string>) exc parameters =
            let wrappedFunction = Helpers.nullValuesToOptions (fun (x: Func<string>) -> (fun () -> x.Invoke())) func
            let wrappedException = Helpers.nullValuesToOptions id exc
            f level wrappedFunction wrappedException (parameters |> List.ofArray)
        LogProvider.SetCurrentLogProvider(QuartzLoggerWrapper(loggerFunction))

    /// <summary>
    /// Sets a LibLog logger directly for Quartz.net
    /// </summary>
    /// <param name="l"> logger</param>
    let SetQuartzLogger l = LogProvider.SetCurrentLogProvider(l)
