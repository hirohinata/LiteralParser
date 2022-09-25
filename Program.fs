// Learn more about F# at http://fsharp.org

open System

type Token =
    | Letter of char
    | Symbol of Symbol * char
    | HexNumber of char
    | Number of char
and Symbol =
    | Sharp
    | Dot
    | Plus
    | Minus
    | Underscore

let Lex (text: string) =
    let rec f (text: string) index length result =
        if length <= index then
            result
        else
            match text.[index] with
            | c when 'A' <= c && c <= 'F' || 'a' <= c && c <= 'f' ->
                f text (index + 1) length (HexNumber c :: result)
            | c when 'G' <= c && c <= 'Z' || 'g' <= c && c <= 'z' ->
                f text (index + 1) length (Letter c :: result)
            | c when '0' <= c && c <= '9' ->
                f text (index + 1) length (Number c :: result)
            | '#' as c ->
                f text (index + 1) length (Symbol(Sharp, c) :: result)
            | '.' as c ->
                f text (index + 1) length (Symbol(Dot, c) :: result)
            | '+' as c ->
                f text (index + 1) length (Symbol(Plus, c) :: result)
            | '-' as c ->
                f text (index + 1) length (Symbol(Minus, c) :: result)
            | '_' as c ->
                f text (index + 1) length (Symbol(Underscore, c) :: result)
            | _ ->
                result
                
    f text 0 text.Length []
    |> List.rev

let rec (|Unsigned_Int|_|) = function
    | Number num1 :: Symbol(Underscore, _) :: Unsigned_Int(num2, xs) -> Some($"{num1}{num2}", xs)
    | Number num1 :: Unsigned_Int(num2, xs) -> Some($"{num1}{num2}", xs)
    | Number num :: xs -> Some($"{num}", xs)
    | _ -> None

let rec (|Hex_Int|_|) = function
    | (Number num1 | HexNumber num1) :: Symbol(Underscore, _) :: Hex_Int(num2, xs) -> Some($"{num1}{num2}", xs)
    | (Number num1 | HexNumber num1) :: Hex_Int(num2, xs) -> Some($"{num1}{num2}", xs)
    | (Number num  | HexNumber num) :: xs -> Some($"{num}", xs)
    | _ -> None

let rec (|Identifier_Continuous|_|) = function
    | (Letter c | HexNumber c | Number c | Symbol(Underscore, c)) :: Identifier_Continuous(s, xs) -> Some($"{c}{s}", xs)
    | (Letter c | HexNumber c | Number c | Symbol(Underscore, c)) :: xs -> Some($"{c}", xs)
    | _ -> None
let (|Identifier|_|) = function
    | (Letter c | HexNumber c | Symbol(Underscore, c)) :: Identifier_Continuous(s, xs) -> Some($"{c}{s}", xs)
    | (Letter c | HexNumber c | Symbol(Underscore, c)) :: xs -> Some($"{c}", xs)
    | _ -> None

let (|Signed_Int|_|) = function
   | Unsigned_Int(num, xs) -> Some(num, xs)
   | Symbol(Plus, _) :: Unsigned_Int(num, xs) -> Some(num, xs)
   | Symbol(Minus, c) :: Unsigned_Int(num, xs) -> Some($"{c}{num}", xs)
   | _ -> None

let (|Base_Number_Specified_Int|_|) = function
    | Unsigned_Int(baseNum, Symbol(Sharp, _) :: Unsigned_Int(num, xs)) -> Some(baseNum, num, xs)
    | Unsigned_Int(baseNum, Symbol(Sharp, _) :: Hex_Int(num, xs)) -> Some(baseNum, num, xs)
    | _ -> None
    
let(|Int_Literal_Core|_|) = function
    | Base_Number_Specified_Int(baseNum, num, xs) -> Some(Some baseNum, num, xs)
    | Signed_Int(num, xs) -> Some(None, num, xs)
    | _ -> None

let(|Int_Literal|_|) = function
    | Identifier(typeName, Symbol(Sharp, _) :: Int_Literal_Core(baseNum, num, xs)) -> Some(Some typeName, baseNum, num, xs)
    | Int_Literal_Core(baseNum, num, xs) -> Some(None, baseNum, num, xs)
    | _ -> None
    
let (|Real_Literal_Core|_|) = function
    | Signed_Int(integral, Symbol(Dot, c) :: Unsigned_Int(fractional, HexNumber e :: Signed_Int(num, xs))) when e = 'e' || e = 'E' ->
        Some($"{integral}{c}{fractional}E{num}", xs)
    | Signed_Int(integral, Symbol(Dot, c) :: Unsigned_Int(fractional, xs)) ->
        Some($"{integral}{c}{fractional}", xs)
    | _ -> None

let (|Real_Literal|_|) = function
    | Identifier(typeName, Symbol(Sharp, _) :: Real_Literal_Core(num, xs)) -> Some(Some typeName, num, xs)
    | Real_Literal_Core(num, xs) -> Some(None, num, xs)
    | _ -> None

let (|Numeric_Literal|_|) = function
    | Real_Literal(typeName, num, xs) -> Some(typeName, None, num, xs)
    | Int_Literal(typeName, baseNum, num, xs) -> Some(typeName, baseNum, num, xs)
    | _ -> None

let (|Fix_Point|_|) = function
    | Unsigned_Int(integral, Symbol(Dot, c) :: Unsigned_Int(fractional, xs)) -> Some($"{integral}{c}{fractional}", xs)
    | Unsigned_Int(num, xs) -> Some(num, xs)
    | _ -> None

let (|Nanoseconds|_|) = function
    | Fix_Point(num, Letter n :: Letter s :: xs) when (n = 'n' || n = 'N') && (s = 's' || s = 'S') ->
        Some($"{num}ns", xs)
    | _ -> None

let (|Microseconds|_|) = function
    | Fix_Point(num, Letter u :: Letter s :: xs) when (u = 'u' || u = 'U') && (s = 's' || s = 'S') ->
        Some($"{num}us", xs)
    | Unsigned_Int(us, Letter u :: Letter s :: Symbol(Underscore, _) :: Nanoseconds(ns, xs)) when (u = 'u' || u = 'U') && (s = 's' || s = 'S') ->
        Some($"{us}us {ns}ns", xs)
    | _ -> None

let (|Interval|_|) = function
    | Microseconds(num, xs) -> Some(num, xs)
    | Nanoseconds(num, xs) -> Some(num, xs)
    | _ -> None

let (|Duration|_|) = function
    | Identifier(typeName, Symbol(Sharp, _) :: Symbol(Plus, _) :: Interval(num, xs)) -> Some(typeName, num, xs)
    | Identifier(typeName, Symbol(Sharp, _) :: Symbol(Minus, c) :: Interval(num, xs)) -> Some(typeName, $"{c}{num}", xs)
    | Identifier(typeName, Symbol(Sharp, _) :: Interval(num, xs)) -> Some(typeName, num, xs)
    | _ -> None

let (|Time_Literal|_|) = function
    | Duration(typeName, num, xs) -> Some(typeName, num, xs)
    | _ -> None

let Parse = function
    | Numeric_Literal(typeName, baseNum, num, []) ->
        let typeName = typeName |> Option.map (fun s -> s + "#") |> Option.defaultValue ""
        let baseNum = baseNum |> Option.map (fun s -> s + "#") |> Option.defaultValue ""
        $"Numeric {typeName}{baseNum}{num}"
    | Time_Literal(typeName, num, []) ->
        $"Time {typeName}#{num}"
    | tokens -> $"Parse Error : {tokens}"

[<EntryPoint>]
let main argv =
    while true do
        stdin.ReadLine()
        |> Lex 
        |> Parse
        |> stdout.WriteLine

    0 // return an integer exit code
