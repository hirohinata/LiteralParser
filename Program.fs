// Learn more about F# at http://fsharp.org

open System

type Symbol =
    | Sharp
    | Dot
    | Plus
    | Minus
    | Underscore
    | Colon

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
        | ':' as c ->
            Symbol(Colon, c, (text, (index + 1), length))
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
    | Unsigned_Int(us, Letter(u, Letter(s, Symbol(Underscore, _, Nanoseconds(ns, xs))))) when (u = 'u' || u = 'U') && (s = 's' || s = 'S') ->
        Some($"{us}us {ns}", xs)
    | Unsigned_Int(us, Letter(u, Letter(s, Nanoseconds(ns, xs)))) when (u = 'u' || u = 'U') && (s = 's' || s = 'S') ->
        Some($"{us}us {ns}", xs)
    | Fix_Point(num, Letter(u, Letter(s, xs))) when (u = 'u' || u = 'U') && (s = 's' || s = 'S') ->
        Some($"{num}us", xs)
    | Nanoseconds(ns, xs) ->
        Some(ns, xs)
    | _ -> None

let (|Milliseconds|_|) = function
    | Unsigned_Int(ms, Letter(m, Letter(s, Symbol(Underscore, _, Microseconds(us, xs))))) when (m = 'm' || m = 'M') && (s = 's' || s = 'S') ->
        Some($"{ms}ms {us}", xs)
    | Unsigned_Int(ms, Letter(m, Letter(s, Microseconds(us, xs)))) when (m = 'm' || m = 'M') && (s = 's' || s = 'S') ->
        Some($"{ms}ms {us}", xs)
    | Fix_Point(num, Letter(m, Letter(s, xs))) when (m = 'm' || m = 'M') && (s = 's' || s = 'S') ->
        Some($"{num}ms", xs)
    | Microseconds(us, xs) ->
        Some(us, xs)
    | _ -> None

let (|Seconds|_|) = function
    | Unsigned_Int(sec, Letter(s, Symbol(Underscore, _, Milliseconds(ms, xs)))) when (s = 's' || s = 'S') ->
        Some($"{sec}s {ms}", xs)
    | Unsigned_Int(sec, Letter(s, Milliseconds(ms, xs))) when (s = 's' || s = 'S') ->
        Some($"{sec}s {ms}", xs)
    | Fix_Point(num, Letter(s, xs)) when (s = 's' || s = 'S') ->
        Some($"{num}s", xs)
    | Milliseconds(ms, xs) ->
        Some(ms, xs)
    | _ -> None

let (|Minutes|_|) = function
    | Unsigned_Int(min, Letter(m, Symbol(Underscore, _, Seconds(sec, xs)))) when (m = 'm' || m = 'M') ->
        Some($"{min}m {sec}", xs)
    | Unsigned_Int(min, Letter(m, Seconds(sec, xs))) when (m = 'm' || m = 'M') ->
        Some($"{min}m {sec}", xs)
    | Fix_Point(num, Letter(m, xs)) when (m = 'm' || m = 'M') ->
        Some($"{num}m", xs)
    | Seconds(sec, xs) ->
        Some(sec, xs)
    | _ -> None

let (|Hours|_|) = function
    | Unsigned_Int(hour, Letter(h, Symbol(Underscore, _, Minutes(min, xs)))) when (h = 'h' || h = 'H') ->
        Some($"{hour}h {min}", xs)
    | Unsigned_Int(hour, Letter(h, Minutes(min, xs))) when (h = 'h' || h = 'H') ->
        Some($"{hour}h {min}", xs)
    | Fix_Point(num, Letter(h, xs)) when (h = 'h' || h = 'H') ->
        Some($"{num}h", xs)
    | Minutes(min, xs) ->
        Some(min, xs)
    | _ -> None

let (|Days|_|) = function
    | Unsigned_Int(day, HexNumber(d, Symbol(Underscore, _, Hours(hour, xs)))) when (d = 'd' || d = 'D') ->
        Some($"{day}d {hour}", xs)
    | Unsigned_Int(day, HexNumber(d, Hours(hour, xs))) when (d = 'd' || d = 'D') ->
        Some($"{day}d {hour}", xs)
    | Fix_Point(num, HexNumber(d, xs)) when (d = 'd' || d = 'D') ->
        Some($"{num}d", xs)
    | Hours(hour, xs) ->
        Some(hour, xs)
    | _ -> None

let (|Interval|_|) = (|Days|_|)

let (|Duration|_|) = function
    | Identifier(typeName, Symbol(Sharp, _, Symbol((Plus | Minus), c, Interval(num, xs)))) -> Some(typeName, $"{c}{num}", xs)
    | Identifier(typeName, Symbol(Sharp, _, Interval(num, xs))) -> Some(typeName, num, xs)
    | _ -> None

let (|Daytime|_|) = function
    | Unsigned_Int(hour, Symbol(Colon, _, Unsigned_Int(min, Symbol(Colon, _, Fix_Point(sec, xs))))) -> Some($"{hour}:{min}:{sec}", xs)
    | _ -> None

let (|Time_Of_Day|_|) = function
    | Identifier(typeName, Symbol(Sharp, _, Daytime(num, xs))) -> Some(typeName, num, xs)
    | _ -> None

let (|Date_Literal|_|) = function
    | Unsigned_Int(year, Symbol(Minus, _, Unsigned_Int(month, Symbol(Minus, _, Unsigned_Int(day, xs))))) -> Some($"{year}-{month}-{day}", xs)
    | _ -> None

let (|Date|_|) = function
    | Identifier(typeName, Symbol(Sharp, _, Date_Literal(num, xs))) -> Some(typeName, num, xs)
    | _ -> None

let (|Date_And_Time|_|) = function
    | Identifier(typeName, Symbol(Sharp, _, Date_Literal(date, Symbol(Minus, _, Daytime(time, xs))))) -> Some(typeName, $"{date}-{time}", xs)
    | _ -> None

let (|Time_Literal|_|) = function
    | Duration(typeName, num, xs) -> Some(typeName, num, xs)
    | Date_And_Time(typeName, num, xs) -> Some(typeName, num, xs)
    | Time_Of_Day(typeName, num, xs) -> Some(typeName, num, xs)
    | Date(typeName, num, xs) -> Some(typeName, num, xs)
    | _ -> None

let (|True_Literal|False_Literal|Unexpected|) (text: string, index, length) =
    let text = text.Substring(index)
    if text.Equals("TRUE", StringComparison.OrdinalIgnoreCase) then
        True_Literal "TRUE"
    elif text.Equals("FALSE", StringComparison.OrdinalIgnoreCase) then
        False_Literal "FALSE"
    else
        Unexpected
let (|Bool_Literal|_|) = function
    | Identifier(typeName, Symbol(Sharp, _, (True_Literal s | False_Literal s))) -> Some(Some typeName, s)
    | (True_Literal s | False_Literal s) -> Some(None, s)
    | _ -> None

let (|Char_Str|_|) (text: string, index, length) =
    if index < length && (text.[index] = '\'' || text.[index] = '"') then
        Some (text.Substring(index))
    else
        None

let (|Char_Literal|_|) = function
    | Identifier(typeName, Symbol(Sharp, _, Char_Str s)) -> Some(Some typeName, s)
    | Char_Str s -> Some(None, s)
    | _ -> None

let Parse text index length =
    let unwrap = Option.map (fun s -> s + "#") >> Option.defaultValue ""

    match text, index, length with
    | Time_Literal(typeName, num, (remainText, index, length)) ->
        if index = length then
            $"Time {typeName}#{num}"
        else
            $"Time Literal Parse Error ( {text} ) : {remainText.Substring(index)} is remained. (index = {index}, length = {length})"
    | Numeric_Literal(typeName, baseNum, num, (remainText, index, length)) ->
        if index = length then
            let typeName = unwrap typeName
            let baseNum = unwrap baseNum
            $"Numeric {typeName}{baseNum}{num}"
        else
            $"Numeric Literal Parse Error ( {text} ) : {remainText.Substring(index)} is remained. (index = {index}, length = {length})"
    | Bool_Literal(typeName, str) ->
        let typeName = unwrap typeName
        $"Bool {typeName}{str}"
    | Char_Literal(typeName, str) ->
        let typeName = unwrap typeName
        $"Char {typeName}{str}"
    | _ -> $"Parse Error ( {text} ) : not match."


[<EntryPoint>]
let main _ =
    while true do
        let s = stdin.ReadLine()
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
        for _ = 0 to 1000 do
            Parse s 0 s.Length
            |> ignore
        stdout.Write sw.Elapsed.TotalMilliseconds
        stdout.WriteLine "us"

    0 // return an integer exit code
