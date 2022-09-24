// Learn more about F# at http://fsharp.org

open System

let Parse text =
    text + "!"

[<EntryPoint>]
let main argv =
    while true do
        stdin.ReadLine()
        |> Parse
        |> stdout.WriteLine

    0 // return an integer exit code
