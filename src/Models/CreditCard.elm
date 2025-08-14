module Models.CreditCard exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)


type alias CreditCard =
    { accountId : String
    , cardNumber : String  -- Last 4 digits only for security
    , cardType : CardType
    , creditLimit : Float
    , availableCredit : Float
    , currentBalance : Float
    , minimumPayment : Float
    , dueDate : String
    , apr : Float
    , rewardsBalance : Maybe Float
    , paymentDueWarning : Bool
    }

type CardType
    = Visa
    | Mastercard
    | Amex
    | Discover
    | Other String

creditCardDecoder : Decoder CreditCard
creditCardDecoder =
    Decode.succeed CreditCard
        |> required "account_id" Decode.string
        |> required "card_number" Decode.string
        |> required "card_type" cardTypeDecoder
        |> required "credit_limit" Decode.float
        |> required "available_credit" Decode.float
        |> required "current_balance" Decode.float
        |> required "minimum_payment" Decode.float
        |> required "due_date" Decode.string
        |> required "apr" Decode.float
        |> optional "rewards_balance" (Decode.maybe Decode.float) Nothing
        |> optional "payment_due_warning" Decode.bool False

cardTypeDecoder : Decoder CardType
cardTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case String.toLower str of
                    "visa" -> Decode.succeed Visa
                    "mastercard" -> Decode.succeed Mastercard
                    "amex" -> Decode.succeed Amex
                    "american_express" -> Decode.succeed Amex
                    "discover" -> Decode.succeed Discover
                    other -> Decode.succeed (Other other)
            )

cardTypeToString : CardType -> String
cardTypeToString cardType =
    case cardType of
        Visa -> "Visa"
        Mastercard -> "Mastercard"
        Amex -> "American Express"
        Discover -> "Discover"
        Other name -> name

utilizationPercentage : CreditCard -> Float
utilizationPercentage card =
    if card.creditLimit > 0 then
        (card.currentBalance / card.creditLimit) * 100
    else
        0

formatCurrency : Float -> String
formatCurrency amount =
    "$" ++ String.fromFloat amount

maskCardNumber : String -> String
maskCardNumber cardNumber =
    "**** **** **** " ++ cardNumber

getUtilizationStatus : CreditCard -> String
getUtilizationStatus card =
    let
        utilization = utilizationPercentage card
    in
    if utilization < 30 then
        "Good"
    else if utilization < 70 then
        "Fair"
    else
        "High"
