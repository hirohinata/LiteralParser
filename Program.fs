// Learn more about F# at http://fsharp.org

open System

type Token =
    | Identifier of str: string
    | Symbol of sym: Symbol
    | HexNumber of num: string
    | Number of num: string
and Symbol =
    | Sharp
    | Dot
    | Plus
    | Minus

let Lex (text: string) =
    let rec f (text: string) index length result =
        if length <= index then
            result
        else
            match text.[index] with
            | c when 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z' ->
                let rec find (text: string) index length isHexNum =
                    if length <= index then
                        index, isHexNum
                    else
                        match text.[index] with
                        | c when '0' <= c && c <= '9' || 'A' <= c && c <= 'F' || 'a' <= c && c <= 'f' || c = '_' ->
                            find text (index + 1) length isHexNum
                        | c when 'G' <= c && c <= 'Z' || 'g' <= c && c <= 'z' ->
                            find text (index + 1) length false
                        | _ -> index, isHexNum
                match find text index length true with
                | next, false -> f text next length (Identifier (text.Substring(index, next - index)) :: result)
                | next, true -> f text next length (HexNumber (text.Substring(index, next - index)) :: result)
            | c when '0' <= c && c <= '9' ->
                let rec find (text: string) index length isHexNum =
                    if length <= index then
                        index, isHexNum
                    else
                        match text.[index] with
                        | c when '0' <= c && c <= '9' || c = '_' ->
                            find text (index + 1) length isHexNum
                        | c when 'A' <= c && c <= 'F' || 'a' <= c && c <= 'f' ->
                            find text (index + 1) length true
                        | _ -> index, isHexNum
                match find text index length false with
                | next, false -> f text next length (Number (text.Substring(index, next - index)) :: result)
                | next, true -> f text next length (HexNumber (text.Substring(index, next - index)) :: result)
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
    
let (|Unsigned_Int|_|) = function
    | Number num :: xs -> Some(num, xs)
    | _ -> None

let (|Signed_Int|_|) = function
   | Unsigned_Int(num, xs) -> Some(num, xs)
   | Symbol Plus :: Unsigned_Int(num, xs) -> Some(num, xs)
   | Symbol Minus:: Unsigned_Int(num, xs) -> Some("-" + num, xs)
   | _ -> None

let (|Base_Number_Specified_Int|_|) = function
    | Number baseNum :: Symbol Sharp :: Unsigned_Int(num, xs) -> Some(baseNum, num, xs)
    | Number baseNum :: Symbol Sharp :: HexNumber num :: xs -> Some(baseNum, num, xs)
    | _ -> None

let(|Int_Literal|_|) = function
    | Base_Number_Specified_Int(baseNum, num, xs) -> Some(baseNum, num, xs)
    | Signed_Int(num, xs) -> Some("10", num, xs)
    | _ -> None

let (|Numeric_Literal|_|) = function
    | Int_Literal(baseNum, num, xs) -> Some(baseNum, num, xs)
    | _ -> None

let Parse = function
    | Int_Literal(baseNum, num, []) -> $"Numeric {baseNum}#{num}"
    | tokens -> $"Parse Error : {tokens}"

[<EntryPoint>]
let main argv =
    while true do
        stdin.ReadLine()
        |> Lex 
        |> Parse
        |> stdout.WriteLine

    0 // return an integer exit code
