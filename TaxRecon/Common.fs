module Common

open Microsoft.FSharp.Reflection
open System.Text.RegularExpressions

module Union =
    let fromString<'a> (s: string) =
        match FSharpType.GetUnionCases typeof<'a>
              |> Array.filter (fun case -> case.Name = s) with
        | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
        | _ -> None
        
module Pattern =
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None
module Math =
    type Sign =
        | Positive
        | Negative
        | Zero
    let toSign d =
        if d < 0 then Negative
        else if d > 0 then Positive
        else Zero