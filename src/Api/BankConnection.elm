module Api.BankConnection exposing (..)

import Http
import Json.Decode as Decode
import Models.Account as Account exposing (Account)
import Models.Transaction as Transaction exposing (Transaction)
import Models.CreditCard as CreditCard exposing (CreditCard)


-- API Configuration
apiBaseUrl : String
apiBaseUrl = "https://your-api-server.com/api"

-- Headers for API requests
authHeaders : List Http.Header
authHeaders =
    [ Http.header "Authorization" "Bearer YOUR_API_TOKEN"
    , Http.header "Content-Type" "application/json"
    ]

-- Connect to bank and retrieve accounts
connectToBank : (Result Http.Error (List Account) -> msg) -> Cmd msg
connectToBank toMsg =
    Http.request
        { method = "GET"
        , headers = authHeaders
        , url = apiBaseUrl ++ "/accounts"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list Account.accountDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

-- Load transactions for a specific account
loadTransactions : String -> (Result Http.Error (List Transaction) -> msg) -> Cmd msg
loadTransactions accountId toMsg =
    Http.request
        { method = "GET"
        , headers = authHeaders
        , url = apiBaseUrl ++ "/accounts/" ++ accountId ++ "/transactions"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list Transaction.transactionDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

-- Load all credit cards
loadAllCreditCards : (Result Http.Error (List CreditCard) -> msg) -> Cmd msg
loadAllCreditCards toMsg =
    Http.request
        { method = "GET"
        , headers = authHeaders
        , url = apiBaseUrl ++ "/credit-cards"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list CreditCard.creditCardDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

-- Load credit card details for a specific account
loadCreditCardDetails : String -> (Result Http.Error CreditCard -> msg) -> Cmd msg
loadCreditCardDetails accountId toMsg =
    Http.request
        { method = "GET"
        , headers = authHeaders
        , url = apiBaseUrl ++ "/credit-cards/" ++ accountId
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg CreditCard.creditCardDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

-- Refresh account data
refreshAccountData : String -> (Result Http.Error Account -> msg) -> Cmd msg
refreshAccountData accountId toMsg =
    Http.request
        { method = "POST"
        , headers = authHeaders
        , url = apiBaseUrl ++ "/accounts/" ++ accountId ++ "/refresh"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg Account.accountDecoder
        , timeout = Just 30000  -- 30 second timeout for refresh operations
        , tracker = Nothing
        }

-- Get account balance history
loadBalanceHistory : String -> Int -> (Result Http.Error (List BalancePoint) -> msg) -> Cmd msg
loadBalanceHistory accountId days toMsg =
    Http.request
        { method = "GET"
        , headers = authHeaders
        , url = apiBaseUrl ++ "/accounts/" ++ accountId ++ "/balance-history?days=" ++ String.fromInt days
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list balancePointDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

-- Helper types for additional API responses
type alias BalancePoint =
    { date : String
    , balance : Float
    }

balancePointDecoder : Decode.Decoder BalancePoint
balancePointDecoder =
    Decode.map2 BalancePoint
        (Decode.field "date" Decode.string)
        (Decode.field "balance" Decode.float)

-- Error handling
errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.Timeout ->
            "Request timeout - please try again"

        Http.NetworkError ->
            "Network error - please check your internet connection"

        Http.BadStatus status ->
            case status of
                401 -> "Authentication failed - please reconnect to your bank"
                403 -> "Access denied - insufficient permissions"
                404 -> "Resource not found"
                429 -> "Too many requests - please wait and try again"
                500 -> "Server error - please try again later"
                _ -> "Server error (status " ++ String.fromInt status ++ ")"

        Http.BadBody body ->
            "Invalid response format: " ++ body

-- Utility function to check if a request should be retried
shouldRetry : Http.Error -> Bool
shouldRetry error =
    case error of
        Http.Timeout -> True
        Http.NetworkError -> True
        Http.BadStatus status -> status >= 500 && status < 600
        _ -> False
