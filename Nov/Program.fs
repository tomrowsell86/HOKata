
let prefix = "iwrupvqb"
let incrementer n = n + 1
let md5Instance = System.Security.Cryptography.MD5.Create()
let posInts = Seq.initInfinite(incrementer)
let kataCoinCheck (a:string) = a.ToCharArray() |> Array.take 5 |> Array.forall (fun c -> c = '0') |> not
let mapper a (b:int) = (md5Instance.ComputeHash(System.Text.Encoding.ASCII.GetBytes(a + (string b))), b) 

let pipeline =  Seq.map (mapper prefix) posInts  |> Seq.map (fun a-> (System.Convert.ToBase64String(fst a),snd a))  |> (Seq.skipWhile (fun (a:string*int)-> kataCoinCheck (fst a))) 
let result = Seq.head pipeline
printfn "%s %d" (fst result) (snd result)  
// For more information see https://aka.ms/fsharp-console-apps

