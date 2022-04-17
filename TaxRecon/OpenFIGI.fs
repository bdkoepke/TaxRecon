module OpenFIGI

open Common.Pattern
open Newtonsoft.Json

[<JsonObject(MemberSerialization = MemberSerialization.Fields)>]
type BBGlobalID = private BBGlobalID of string

module BBGlobalID =
    let toString (BBGlobalID id) = id

    let checkDigit (b: BBGlobalID) =
        let mapCode (l: char) =
            match (string l) with
            | Regex "[A-Z-[A,E,I,O,U]]" [] -> int l - ((int 'A') - 10) |> Some
            | Regex "[0-9]" [] -> int l - (int '0') |> Some
            | _ -> None

        let xs = toString b |> List.ofSeq

        let acc =
            xs.[..xs.Length - 2]
            |> List.mapi (fun i x -> i, x)
            |> List.fold
                (fun acc (i, x) ->
                    match acc, mapCode x with
                    | Some acc', Some x' ->
                        acc'
                        + ((string (x' * (if i % 2 = 0 then 1 else 2))
                            |> List.ofSeq
                            |> List.map (fun y -> int y - int '0'))
                           |> List.sum)
                        |> Some
                    | _, _ -> None)
                (Some 0)

        match acc with
        | Some x ->
            let expected = (int (List.last xs) - (int '0'))
            let actual = (10 - x % 10) % 10
            actual = expected
        | None -> false

    let fromString s =
        match s with
        | Regex "((?!BS|BM|GG|GB|GH|KY|VG)[[A-Z,0-9-[A,E,I,O,U]]{2}G[A-Z,0-9-[A,E,I,O,U]]{8}[0-9])" [ x ] ->
            let b = BBGlobalID(x)

            match checkDigit b with
            | true -> Some b
            | false -> None
        | _ -> None
