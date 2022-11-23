// For more information see https://aka.ms/fsharp-console-apps
type NaughtOrNiceCheck =
    { VowelCheck: bool
      DoubleCheck: bool
      ForbiddenLetterCheck: bool
      Expression: string }

let lines = System.IO.File.ReadLines("data.txt")
let vowels = [ 'a'; 'e'; 'i'; 'o'; 'u' ]
let forbiddenStrings = [ "ab"; "cd"; "pq"; "xy" ]
let forbiddenStringCheck input = List.contains input forbiddenStrings
let vowelCheck c = List.contains c vowels

let vowelCountMapper (input: string) =
    input.ToCharArray()
    |> Array.map vowelCheck
    |> Array.where (fun x -> x)
    |> Array.length

let doubleLetterCheck (x: string) =
    let rec innerCheck list =
        match list with
        | x :: y :: tl -> if (x = y) then true else innerCheck (y :: tl)
        | [ _ ]
        | [] -> false
    innerCheck (Array.toList (x.ToCharArray()))

let filteredSet =
    Seq.map
        (fun x ->
            { Expression = x
              VowelCheck = ((vowelCountMapper x) > 3)
              DoubleCheck = (doubleLetterCheck x)
              ForbiddenLetterCheck = (forbiddenStringCheck x) })
        lines

printfn "Lines found %d" (Seq.length lines)

let result =
    Seq.where (fun x -> x.VowelCheck && not (x.ForbiddenLetterCheck) && x.DoubleCheck) filteredSet

Seq.iter (fun x -> printfn "%s" x.Expression) result
printfn "%d" (Seq.length result)
