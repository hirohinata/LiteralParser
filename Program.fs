// Learn more about F# at http://fsharp.org

open System

type Symbol =
    | Sharp
    | Dot
    | Plus
    | Minus
    | Underscore

let (|Letter|Symbol|HexNumber|Number|Unexpected|) (text: string, index, length) =
    if length <= index then
        Unexpected
    else
        match text.[index] with
        | c when 'A' <= c && c <= 'F' || 'a' <= c && c <= 'f' ->
            HexNumber(c, (text, (index + 1), length))
        | c when 'G' <= c && c <= 'Z' || 'g' <= c && c <= 'z' ->
            Letter(c, (text, (index + 1), length))
        | c when '0' <= c && c <= '9' ->
            Number(c, (text, (index + 1), length))
        | '#' as c ->
            Symbol(Sharp, c, (text, (index + 1), length))
        | '.' as c ->
            Symbol(Dot, c, (text, (index + 1), length))
        | '+' as c ->
            Symbol(Plus, c, (text, (index + 1), length))
        | '-' as c ->
            Symbol(Minus, c, (text, (index + 1), length))
        | '_' as c ->
            Symbol(Underscore, c, (text, (index + 1), length))
        | _ ->
            Unexpected

let rec (|Unsigned_Int|_|) = function
    | Number(num1, Symbol(Underscore, _, Unsigned_Int(num2, xs))) -> Some($"{num1}{num2}", xs)
    | Number(num1, Unsigned_Int(num2, xs)) -> Some($"{num1}{num2}", xs)
    | Number(num, xs) -> Some($"{num}", xs)
    | _ -> None

let rec (|Hex_Int|_|) = function
    | Number(num1, Symbol(Underscore, _, Hex_Int(num2, xs))) -> Some($"{num1}{num2}", xs)
    | HexNumber(num1, Symbol(Underscore, _, Hex_Int(num2, xs))) -> Some($"{num1}{num2}", xs)
    | Number(num1, Hex_Int(num2, xs)) -> Some($"{num1}{num2}", xs)
    | HexNumber(num1, Hex_Int(num2, xs)) -> Some($"{num1}{num2}", xs)
    | Number(num, xs) -> Some($"{num}", xs)
    | HexNumber(num, xs) -> Some($"{num}", xs)
    | _ -> None
    
let (|Identifier_Lead_Token|_|) = function
    | Letter(c, xs)
    | HexNumber(c, xs)
    | Symbol(Underscore, c, xs)
        -> Some(c, xs)
    | _ -> None
let (|Identifier_Trail_Token|_|) = function
    | Letter(c, xs)
    | HexNumber(c, xs)
    | Number(c, xs)
    | Symbol(Underscore, c, xs)
        -> Some(c, xs)
    | _ -> None
let rec (|Identifier_Continuous|_|) = function
    | Identifier_Trail_Token(c, Identifier_Continuous(s, xs)) -> Some($"{c}{s}", xs)
    | Identifier_Trail_Token(c, xs) -> Some($"{c}", xs)
    | _ -> None
let (|Identifier|_|) = function
    | Identifier_Lead_Token(c, Identifier_Continuous(s, xs)) -> Some($"{c}{s}", xs)
    | Identifier_Lead_Token(c, xs) -> Some($"{c}", xs)
    | _ -> None

let (|Signed_Int|_|) = function
   | Unsigned_Int(num, xs) -> Some(num, xs)
   | Symbol((Plus | Minus), c, Unsigned_Int(num, xs)) -> Some($"{c}{num}", xs)
   | _ -> None

let (|Base_Number_Specified_Int|_|) = function
    | Unsigned_Int(baseNum, Symbol(Sharp, _, Unsigned_Int(num, xs))) -> Some(baseNum, num, xs)
    | Unsigned_Int(baseNum, Symbol(Sharp, _, Hex_Int(num, xs))) -> Some(baseNum, num, xs)
    | _ -> None
    
let(|Int_Literal_Core|_|) = function
    | Base_Number_Specified_Int(baseNum, num, xs) -> Some(Some baseNum, num, xs)
    | Signed_Int(num, xs) -> Some(None, num, xs)
    | _ -> None

let(|Int_Literal|_|) = function
    | Identifier(typeName, Symbol(Sharp, _, Int_Literal_Core(baseNum, num, xs))) -> Some(Some typeName, baseNum, num, xs)
    | Int_Literal_Core(baseNum, num, xs) -> Some(None, baseNum, num, xs)
    | _ -> None
    
let (|Real_Literal_Core|_|) = function
    | Signed_Int(integral, Symbol(Dot, c, Unsigned_Int(fractional, HexNumber(e, Signed_Int(num, xs))))) when e = 'e' || e = 'E' ->
        Some($"{integral}{c}{fractional}E{num}", xs)
    | Signed_Int(integral, Symbol(Dot, c, Unsigned_Int(fractional, xs))) ->
        Some($"{integral}{c}{fractional}", xs)
    | _ -> None

let (|Real_Literal|_|) = function
    | Identifier(typeName, Symbol(Sharp, _, Real_Literal_Core(num, xs))) -> Some(Some typeName, num, xs)
    | Real_Literal_Core(num, xs) -> Some(None, num, xs)
    | _ -> None

let (|Numeric_Literal|_|) = function
    | Real_Literal(typeName, num, xs) -> Some(typeName, None, num, xs)
    | Int_Literal(typeName, baseNum, num, xs) -> Some(typeName, baseNum, num, xs)
    | _ -> None

let (|Fix_Point|_|) = function
    | Unsigned_Int(integral, Symbol(Dot, c, Unsigned_Int(fractional, xs))) -> Some($"{integral}{c}{fractional}", xs)
    | Unsigned_Int(num, xs) -> Some(num, xs)
    | _ -> None

let (|Nanoseconds|_|) = function
    | Fix_Point(num, Letter(n, Letter(s, xs))) when (n = 'n' || n = 'N') && (s = 's' || s = 'S') ->
        Some($"{num}ns", xs)
    | _ -> None

let (|Microseconds|_|) = function
    | Fix_Point(num, Letter(u, Letter(s, xs))) when (u = 'u' || u = 'U') && (s = 's' || s = 'S') ->
        Some($"{num}us", xs)
    | Unsigned_Int(us, Letter(u, Letter(s, Symbol(Underscore, _, Nanoseconds(ns, xs))))) when (u = 'u' || u = 'U') && (s = 's' || s = 'S') ->
        Some($"{us}us {ns}", xs)
    | _ -> None

let (|Interval|_|) = function
    | Microseconds(num, xs) -> Some(num, xs)
    | Nanoseconds(num, xs) -> Some(num, xs)
    | _ -> None

let (|Duration|_|) = function
    | Identifier(typeName, Symbol(Sharp, _, Symbol((Plus | Minus), c, Interval(num, xs)))) -> Some(typeName, $"{c}{num}", xs)
    | Identifier(typeName, Symbol(Sharp, _, Interval(num, xs))) -> Some(typeName, num, xs)
    | _ -> None

let (|Time_Literal|_|) = function
    | Duration(typeName, num, xs) -> Some(typeName, num, xs)
    | _ -> None

let Parse text index length =
    match text, index, length with
    | Time_Literal(typeName, num, (remainText, index, length)) ->
        if index = length then
            $"Time {typeName}#{num}"
        else
            $"Time Literal Parse Error ( {text} ) : {remainText.Substring(index)} is remained. (index = {index}, length = {length})"
    | Numeric_Literal(typeName, baseNum, num, (remainText, index, length)) ->
        if index = length then
            let typeName = typeName |> Option.map (fun s -> s + "#") |> Option.defaultValue ""
            let baseNum = baseNum |> Option.map (fun s -> s + "#") |> Option.defaultValue ""
            $"Numeric {typeName}{baseNum}{num}"
        else
            $"Numeric Literal Parse Error ( {text} ) : {remainText.Substring(index)} is remained. (index = {index}, length = {length})"
    | _ -> $"Parse Error ( {text} ) : not match."


[<EntryPoint>]
let main argv =
    while true do
        let s = stdin.ReadLine()
        Parse s 0 s.Length
        |> stdout.WriteLine

    0 // return an integer exit code
