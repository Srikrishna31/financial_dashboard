module Models.Account exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)


type alias Account =
    { id : String
    , name : String
    , accountType : AccountType
    , balance : Float
    , availableBalance : Maybe Float
    , currency : String
    , lastUpdated : String
    , institutionName : String
    }

type AccountType
    = Checking
    | Savings
    | CreditCard
    | Investment
    | Loan

accountDecoder : Decoder Account
accountDecoder =
    Decode.succeed Account
        |> required "account_id" Decode.string
        |> required "name" Decode.string
        |> required "type" accountTypeDecoder
        |> required "balance" Decode.float
        |> optional "available_balance" (Decode.maybe Decode.float) Nothing
        |> optional "currency" Decode.string "USD"
        |> required "last_updated" Decode.string
        |> optional "institution_name" Decode.string "Unknown Bank"

accountTypeDecoder : Decoder AccountType
accountTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case String.toLower str of
                    "checking" -> Decode.succeed Checking
                    "savings" -> Decode.succeed Savings
                    "credit_card" -> Decode.succeed CreditCard
                    "credit" -> Decode.succeed CreditCard
                    "investment" -> Decode.succeed Investment
                    "loan" -> Decode.succeed Loan
                    _ -> Decode.fail ("Unknown account type: " ++ str)
            )

accountTypeToString : AccountType -> String
accountTypeToString accountType =
    case accountType of
        Checking -> "Checking"
        Savings -> "Savings"
        CreditCard -> "Credit Card"
        Investment -> "Investment"
        Loan -> "Loan"

formatBalance : Account -> String
formatBalance account =
    let
        formatted = String.fromFloat account.balance
        symbol = case account.currency of
            "USD" -> "$"
            "EUR" -> "€"
            "GBP" -> "£"
            _ -> account.currency ++ " "
    in
    symbol ++ formatted
