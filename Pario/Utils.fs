namespace Pario

module Utils =

    type 'a Try =
        | Success of 'a 
        | Failure of exn

    module Try =
        let bind f a =
            match a with
            | Success a -> f a
            | Failure e -> Failure e

        let combine f g =
            f |> bind (fun () -> g)

        type Builder() =
            member x.Bind(a, f) = bind f a
            member x.ReturnFrom a = a
            member x.Combine f g = combine f g

    type StringTokenizer(separators:char[]) =

        new() =
            StringTokenizer(StringTokenizer.DefaultSeparators)

        member this.Tokenize (input:string) =
            input.Split(separators) |> Array.toList 

        static member DefaultSeparators = 
            [|  '\t'; 
                '\n';
                '\r';
                '\f';
                ' ';
            |]

    let defaultTokenizer = StringTokenizer()

    let tokenize input = 
        defaultTokenizer.Tokenize input

