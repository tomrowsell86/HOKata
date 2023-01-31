open System

let primeNumberDetector (n: int) =
    let rec primeCheck (candidate: int) (divider: int) =
        if (candidate / 2 >= divider) then
            if (candidate % divider) <> 0 then
                primeCheck candidate (divider + 1)
            else
                false
        else
            true

    match n with
    | 1 -> false
    | x -> primeCheck x 2

let primeNumberPrinter (input: string) =

    let toInt (x: char) =
        let mutable y: int = 0
        let result = Int32.TryParse(x.ToString(), &y)
        if not (result) then (result, None) else (result, Some(y))

    let numberCombinator (values: list<int>) =
        match values with
        | _ :: tl ->

            let outerFold (state: list<int> * list<int>) (x: int) =
                let innerFold (state: int) (value: int) =
                    let result = (state * 10 + value)
                    result

                let (result, tailInts) = state

                match tailInts with
                | _ :: tl -> (result @ List.scan innerFold x tailInts, tl)
                | [] -> state

            let (res, _) = List.fold outerFold ([], tl) values
            res

        | [] -> raise (SystemException("Empty list passed"))

    let numberChunker (input: string) =

        let folder (current: char) (state: list<list<int>> * list<int>) =
            let (numbers, currentRun) = state
            let (isInt, intVal) = toInt current

            if isInt then
                (numbers, intVal.Value :: currentRun)
            else if List.length currentRun > 0 then
                ((currentRun :: numbers), [])
            else
                (numbers, [])

        let (result, lastGroup) = Array.foldBack folder (input.ToCharArray()) ([], [])
        List.where (fun x -> List.length x > 0) (result @ [ lastGroup ])

    numberChunker input
    |> List.collect (fun x -> numberCombinator x)
    |> List.where primeNumberDetector
    |> List.distinct

printfn "Enter a number:"
let input = Console.ReadLine()
let result = primeNumberPrinter input

List.iter (fun x -> printfn "%d" x) result
