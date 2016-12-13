open System
open System.IO
open FParsec

type Parser<'a> = char list -> ('a * char list) option

let path = @"/home/jacque/Downloads/rosalind_dna.txt"
let file = File.ReadAllText(path)

let runParser (p : Parser<'a>) (input : string) =
    Seq.toList input
    |> p

let evalParser (p : Parser<'a>) (input : string) =
    runParser p input |> Option.map fst


let superSimple (ret : 'a) : Parser<'a> =
    fun input -> Some (ret, input)


let sample<'T,'u> = pstring "hello"

let parseAny : Parser<char> = 
    fun input -> 
        match input with
        | [] -> None
        | (c::cs) -> Some (c, cs)

//test on a 
runParser parseAny "hello" |> printfn "%A"

runParser parseAny file |> printfn "%A"

///Let's filter out all the 'A's in the text file of the DNA string
let parseOne (filt : char -> bool) : Parser<char> =
   fun input ->
      match input with
      | [] -> None
      | (x::xs) when filt x -> Some (x,xs)
      | _ -> None

runParser (parseOne ((=) 'H')) "Hello" |> printfn "%A"
runParser (parseOne ((=) 'l')) "Hello" |> printfn "%A"
runParser (parseOne ((=) 'H')) "World" |> printfn "%A"
runParser (parseOne((=) 'A')) file |> printfn "%A"
evalParser (parseOne((=) 'H')) "Hello" |> printfn "%A"
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

