namespace Pario

module Http =
    open System
    open System.Web
    open System.Text
    open System.Net
    open System.IO

    let defaultEncoding = new UTF8Encoding()

    let extensionToContentType (ext:string) =
        match ext with
        | ".jpg" -> "image/jpeg"
        | ".js" -> "text/javascript"
        | ".html" -> "text/html"
        | ".css" -> "text/css"
        | ".png" -> "image/png"
        | ".min.js" -> "application/javascript"
        | ".gif" -> "image/gif"
        | _ -> "application/octet-stream"

    module Server =
        type HttpHandlerResult =
            | Response of unit
            | Pass

        type HttpHandler = HttpListenerRequest -> HttpListenerResponse -> HttpHandlerResult

        type HttpServer(host:string, channels: HttpHandler list) =
            let handler (req:HttpListenerRequest) (resp:HttpListenerResponse) =
                async {
                    let res = 
                        List.fold 
                        <| fun acc handler ->
                            match acc with
                            | Response a -> Response()
                            | Pass -> handler req resp
                        <| Pass 
                        <| channels
                    match res with
                    | Response _ -> ()
                    | Pass -> ()
                }

            let listener (handler:(HttpListenerRequest->HttpListenerResponse->Async<unit>)) =
                let hl = new HttpListener()
                hl.Prefixes.Add host

                hl.Start()
                let task = Async.FromBeginEnd(hl.BeginGetContext, hl.EndGetContext)
                async {
                    while true do
                        let! context = task
                        Async.Start(handler context.Request context.Response)
                } |> Async.Start
            
            member this.Start() =
                listener handler

    module Response =
        let responseFunction f (resp:HttpListenerResponse) =
            f resp

        let responseHeader name values =
            responseFunction (fun resp ->
                List.iter (fun value -> resp.AddHeader(name, value)) values
            )

        let headerName name =
            fun value ->
                responseHeader name [ value ]

        let acceptRanges = headerName "Accept-Ranges"

        let age = headerName "Age"

        let allow = headerName "Allow"

        let cacheControl = headerName "Cache-Control"

        let connection = headerName "Connection"

        let contentEncoding = headerName "Content-Encoding"

        let contentLength = headerName "Content-Length"

        let contentType = headerName "Content-Type"

        let date = headerName "Date"

        let location = headerName "Location"

        let responseBytes data =
            responseFunction
            <| fun resp ->
                resp.OutputStream.Write(data, 0, data.Length)

        let responseString (data:string) =
            responseFunction
            <| fun resp ->
                use writer = new StreamWriter(resp.OutputStream, defaultEncoding)
                writer.Write(data)

        let badRequest = 
            responseFunction
            <| fun resp ->
                resp.StatusCode <- 404

        let responseResource root host url =
            responseFunction
            <| fun resp ->
                let path = Path.Combine(root, Uri(host).MakeRelativeUri(url).OriginalString)
                if File.Exists path then
                    let contentType = 
                        let name = url.AbsolutePath
                        let i = name.IndexOf('.')
                        if i = -1 then
                            extensionToContentType ""
                        else 
                            let ext = name.Substring(i, name.Length - i)
                            extensionToContentType ext
                            
                    use stream = File.OpenRead(path)

                    resp.ContentType <- contentType
                    resp.ContentLength64 <- stream.Length

                    stream.CopyTo(resp.OutputStream)
                else
                    badRequest resp

        let respond opts (resp:HttpListenerResponse) =
            List.iter (fun f -> f resp) opts
            resp.Close()

    module Request =
        let requestFunction f (req:HttpListenerRequest) =
            f req

        let (|Params|_|) (key:string) =
            requestFunction 
            <| fun req ->
                req.QueryString.GetValues(key)
                |> Array.toList 
                |> Some

        let (|RequestHeader|_|) (name:string) =
            requestFunction
            <| fun req ->
                match req.Headers.GetValues(name) |> Array.toList with
                | [] -> None
                | l::_ -> Some l

        let (|Body|_|) =
            requestFunction
            <| fun req ->
                use stream = req.InputStream
                use mem = new MemoryStream()
                stream.CopyTo(mem)
                mem.GetBuffer() |> Some

        let (|StringBody|_|) req =
            match req with
            | Body bytes ->
                defaultEncoding.GetString(bytes) |> Some
            | _ -> None

        let (|StringHeader|_|) name req =
            match req with
            | RequestHeader name header -> Some header
            | _ -> None

        let (|IntHeader|_|) name req =
            match req with
            | RequestHeader name header -> 
                let mutable x = 0
                match Int32.TryParse(header, &x) with
                | true -> Some x
                | false -> None
            | _ -> None

        let (|Authorization|_|) req =
            match req with
            | StringHeader "Authorization" header -> Some header
            | _ -> None

        let (|BasicAuth|_|) req =
            match req with
            | Authorization header ->
                match Utils.tokenize header with
                | [] -> None
                | a::[] -> None
                | "Basic"::auth::[] -> 
                    let data = 
                        System.Convert.FromBase64String(auth)
                        |> defaultEncoding.GetString
                    match data.Split([| ':' |], 2) |> Array.toList with
                    | [] -> None
                    | u::[] -> None
                    | u::p::[] -> Some (u, p)
                    | _ -> None
                | _ -> None
            | _ -> None
                

        let (|Method|_|) httpMethod =
            requestFunction 
            <| fun req ->
                if req.HttpMethod = httpMethod then
                    Some()
                else None

        let (|Get|_|) req = 
            match req with
            | Method "GET" _ -> Some()
            | _ -> None

        let (|Post|_|) req =
            match req with
            | Method "POST" _ -> Some()
            | _ -> None

        let (|Put|_|) req = 
            match req with
            | Method "PUT" _ -> Some()
            | _ -> None

        let (|Delete|_|) req = 
            match req with
            | Method "DELETE" _ -> Some()
            | _ -> None

        let (|Head|_|) req = 
            match req with
            | Method "HEAD" _ -> Some()
            | _ -> None

        let (|Connect|_|) req = 
            match req with
            | Method "CONNECT" _ -> Some()
            | _ -> None

        let (|Options|_|) req = 
            match req with
            | Method "OPTIONS" _ -> Some()
            | _ -> None
            