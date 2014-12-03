namespace Pario

open System
open System.Net
open System.Text

module Web =
    let inline request (uri:Uri) =
        WebRequest.CreateHttp(uri)

    let inline webRequestFunction f (req:HttpWebRequest) =
        f req

    let inline httpMethod methodT =
        webRequestFunction
        <| fun req ->
            req.Method <- methodT
            req

    let inline httpGet req = httpMethod "GET" req
    let inline httpPost req = httpMethod "POST" req
    let inline httpPut req = httpMethod "PUT" req

    let inline contentLength length = 
        webRequestFunction
        <| fun req ->
            req.ContentLength <- length
            req

    let inline contentType cType =
        webRequestFunction
        <| fun req ->
            req.ContentType <- cType
            req

    let inline acceptHeader accept = 
        webRequestFunction
        <| fun req ->
            req.Accept <- accept
            req

    let inline rangeHeaderInt (range:int) =
        webRequestFunction
        <| fun req ->
            req.AddRange(range)
            req

    let inline rangeHeaderFromTo (fromRange:int) (toRange:int) =
        webRequestFunction
        <| fun req ->
            req.AddRange(fromRange, toRange)
            req

    let inline asStream() =
        webRequestFunction
        <| fun req ->
            req.GetResponse().GetResponseStream()

    let inline useStream f =
        webRequestFunction
        <| fun req -> 
            use resp = req.GetResponse()
            use input = resp.GetResponseStream()
            f input

    let inline useStreamAsync f =
        webRequestFunction
        <| fun req ->
            async {
                use! resp = req.GetResponseAsync() |> Async.AwaitTask
                use input = resp.GetResponseStream()

                f input
            } |> Async.Start
        


