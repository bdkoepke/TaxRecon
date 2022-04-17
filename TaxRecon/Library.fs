open FSharp.Data
open System
open Newtonsoft.Json

open Common.Math
open Common
open OpenFIGI

type Transactions = CsvProvider<"Transactions.csv", PreferOptionals=true>

type Currency = CAD of decimal

type TransactionType =
    | OpeningACB
    | Trade
    | TransferIn
    | TransferOut

type Account = string

type Transaction =
    { TradeDate: DateTime
      BBGlobalID: BBGlobalID
      Quantity: int
      Price: Currency
      Transaction: TransactionType
      Account: Account }

type TaxableTransaction =
    { Transaction: Transaction
      ACB: Currency }

type Position =
    { BBGlobalID: BBGlobalID
      Quantity: int
      ACB: Currency
      Account: Account }

let loadTransactions (filePath: string) =
    let ts = Transactions.Load(filePath)

    ts.Rows
    |> Seq.map
        (fun t ->
            match BBGlobalID.fromString t.Id_bb_global, Union.fromString<TransactionType> (t.Transaction) with
            | Some bbGlobalId, Some transaction ->
                { TradeDate = t.``Date/Time``
                  BBGlobalID = bbGlobalId
                  Quantity = t.Quantity
                  Price = Currency.CAD t.Price
                  Transaction = transaction
                  Account = t.Account }
                |> Some
            | _, _ -> None)

open System.IO

[<EntryPoint>]
let main args =
    let ts =
        loadTransactions ("Transactions.csv")

    let transactions =
        ts
        |> List.ofSeq
        |> List.fold
            (fun acc x ->
                match x with
                | Some x' -> x' :: acc
                | _ -> failwith "Transaction failed to parse")
            []
        |> List.sortBy (fun x -> x.TradeDate)
        
    let toPosition (transaction: Transaction) =
        { BBGlobalID = transaction.BBGlobalID
          Quantity = transaction.Quantity
          ACB =
              match transaction.Price with
              | CAD x -> x * (decimal transaction.Quantity) |> CAD
          Account = transaction.Account }
        
    let upsertPosition positions (transaction: Transaction) (position: Position) =
        positions 
        |> Map.add (transaction.Account, transaction.BBGlobalID) position
        
    let updateACB (position: Position) (transaction: Transaction) =
        match position.ACB, transaction.Price with
        | CAD x, CAD y -> CAD(x + y * (decimal transaction.Quantity))
    
    let updatePosition (position: Position) (transaction: Transaction) =
        { position with
              Quantity = position.Quantity + transaction.Quantity
              ACB = updateACB position transaction }

    let xs =
        transactions
        |> List.fold
            (fun (positions, taxableTransactions) transaction ->
                match transaction.Transaction with
                | OpeningACB
                | TransferIn
                | Trade ->
                    let maybePosition = positions
                                        |> Map.tryFind (transaction.Account, transaction.BBGlobalID)
                    match maybePosition with
                    | None -> upsertPosition positions transaction (toPosition transaction), taxableTransactions
                    | Some currentPosition ->
                        match toSign currentPosition.Quantity, toSign transaction.Quantity with
                        | _, Zero -> positions, taxableTransactions
                        | Zero, _ -> upsertPosition positions transaction (toPosition transaction), taxableTransactions
                        | Positive, Positive
                        | Negative, Negative -> upsertPosition positions transaction (updatePosition currentPosition transaction), taxableTransactions
                        | Positive, Negative
                        | Negative, Positive ->
                            let updatedQuantity =
                                currentPosition.Quantity + transaction.Quantity

                            let updatedPositions =
                                match updatedQuantity with
                                | 0 ->
                                    positions
                                    |> Map.remove (transaction.Account, transaction.BBGlobalID)
                                | _ ->
                                    let updatedPosition =
                                        { currentPosition with
                                              Quantity = updatedQuantity
                                              ACB = updateACB currentPosition transaction }
                                    upsertPosition positions transaction updatedPosition

                            let taxableQuantity =
                                let min =
                                    [ currentPosition.Quantity
                                      transaction.Quantity ]
                                    |> List.map abs
                                    |> List.min

                                min * Math.Sign(transaction.Quantity)

                            let acb =
                                match currentPosition.ACB with
                                (* Bug for negative prices... *)
                                | CAD x ->
                                    CAD(
                                        Math.Abs(x / (decimal currentPosition.Quantity))
                                        * -(decimal taxableQuantity)
                                    )

                            updatedPositions,
                            { Transaction =
                                  { transaction with
                                        Quantity = taxableQuantity }
                              ACB = acb }
                            :: taxableTransactions
                | TransferOut ->
                    let maybePosition =
                        positions
                        |> Map.tryFind (transaction.Account, transaction.BBGlobalID)
                    match maybePosition with
                    | Some currentPosition ->
                        let quantity =
                            currentPosition.Quantity + transaction.Quantity

                        let updatedPositions =
                            match quantity with
                            | 0 ->
                                positions
                                |> Map.remove (transaction.Account, transaction.BBGlobalID)
                            | _ ->
                                let updatedPosition =
                                    { currentPosition with
                                          Quantity = quantity
                                          ACB =
                                              match currentPosition.ACB, transaction.Price with
                                              | CAD x, CAD y -> CAD(x + y * (decimal transaction.Quantity)) }

                                positions
                                |> Map.add (transaction.Account, transaction.BBGlobalID) updatedPosition

                        updatedPositions, taxableTransactions
                    | _ -> failwith "todo")
            (Map.empty, [])

    let basis, taxableTransactions = xs
    use sw = File.CreateText("basis.json")

    JsonSerializer
        .Create()
        .Serialize(
            sw,
            {| Basis = Map.values basis
               TaxableTransactions = taxableTransactions |}
        )

    printf $"%A{xs}"
    0
