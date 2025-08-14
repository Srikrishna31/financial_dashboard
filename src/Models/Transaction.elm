module Models.Transaction exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)


type alias Transaction =
    { id : String
    , accountId : String
    , amount : Float
    , description : String
    , date : String
    , category : String
    , transactionType : TransactionType
    , pending : Bool
    , merchantName : Maybe String
    }

type TransactionType
    = Debit
    | Credit

transactionDecoder : Decoder Transaction
transactionDecoder =
    Decode.succeed Transaction
        |> required "transaction_id" Decode.string
        |> required "account_id" Decode.string
        |> required "amount" Decode.float
        |> required "description" Decode.string
        |> required "date" Decode.string
        |> optional "category" Decode.string "Other"
        |> required "type" transactionTypeDecoder
        |> optional "pending" Decode.bool False
        |> optional "merchant_name" (Decode.maybe Decode.string) Nothing

transactionTypeDecoder : Decoder TransactionType
transactionTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case String.toLower str of
                    "debit" -> Decode.succeed Debit
                    "credit" -> Decode.succeed Credit
                    _ -> Decode.fail ("Unknown transaction type: " ++ str)
            )

formatAmount : Transaction -> String
formatAmount transaction =
    let
        prefix = case transaction.transactionType of
            Debit -> "-$"
            Credit -> "+$"
        amount = abs transaction.amount |> String.fromFloat
    in
    prefix ++ amount

isRecent : String -> Bool
isRecent dateString =
    -- This would need proper date parsing in a real application
    -- For now, just check if it's from the last 30 days
    True

categorizeTransaction : Transaction -> String
categorizeTransaction transaction =
    let
        desc = String.toLower transaction.description
    in
    if String.contains "grocery" desc || String.contains "supermarket" desc then
        "Groceries"
    else if String.contains "gas" desc || String.contains "fuel" desc then
        "Transportation"
    else if String.contains "restaurant" desc || String.contains "coffee" desc then
        "Dining"
    else if String.contains "amazon" desc || String.contains "target" desc then
        "Shopping"
    else
        transaction.category
