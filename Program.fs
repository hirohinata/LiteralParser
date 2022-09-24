// Learn more about F# at http://fsharp.org

open System

type Token =
    | Identifier of str: string
    | Symbol of sym: Symbol
    | Number of num: string
and Symbol =
    | Sharp
    | Dot
    | Plus
    | Minus

let Lex (text: string) =
    let rec find (text: string) index length cnd =
        if length <= index then
            index
        elif cnd text.[index] then
            find text (index + 1) length cnd
        else
            index
    let rec f (text: string) index length result =
        if length <= index then
            result
        else
            match text.[index] with
            | c when 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z' ->
                let next = find text index length (fun c -> 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z')
                f text next length (Identifier (text.Substring(index, next - index)) :: result)
            | c when '0' <= c && c <= '9' ->
                let next = find text index length (fun c -> '0' <= c && c <= '9')
                f text next length (Number (text.Substring(index, next - index)) :: result)
            | '#' ->
                f text (index + 1) length (Symbol Sharp :: result)
            | '.' ->
                f text (index + 1) length (Symbol Dot :: result)
            | '+' ->
                f text (index + 1) length (Symbol Plus :: result)
            | '-' ->
                f text (index + 1) length (Symbol Minus :: result)
            | _ ->
                result
                
    f text 0 text.Length []
    |> List.rev
    
let (|Unsigned_Int|_|)= function
    | Number num :: xs -> Some(num, xs)
    | _ -> None

let (|Signed_Int|_|)= function
   | Unsigned_Int(num, xs) -> Some(num, xs)
   | Symbol Plus :: Unsigned_Int(num, xs) -> Some(num, xs)
   | Symbol Minus:: Unsigned_Int(num, xs) -> Some("-" + num, xs)
   | _ -> None

let(|Int_Literal|_|) = function
    | Signed_Int(num, xs) -> Some(num, xs)
    | _ -> None

let (|Numeric_Literal|_|) = function
    | Int_Literal(num, xs) -> Some(num, xs)
    | _ -> None

let Parse = function
    | Numeric_Literal(num, []) -> $"Numeric {num}"
    | tokens -> $"Parse Error : {tokens}"

[<EntryPoint>]
let main argv =
    while true do
        stdin.ReadLine()
        |> Lex 
        |> Parse
        |> stdout.WriteLine

    0 // return an integer exit code
