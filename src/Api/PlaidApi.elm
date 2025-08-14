module Api.PlaidApi exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Models.Account as Account exposing (Account)
import Models.Transaction as Transaction exposing (Transaction)


-- Plaid API Configuration
plaidBaseUrl : String
plaidBaseUrl = "https://production.plaid.com"  -- Use sandbox for development

plaidClientId : String
plaidClientId = "YOUR_PLAID_CLIENT_ID"

plaidSecret : String
plaidSecret = "YOUR_PLAID_SECRET"

-- Plaid API Headers
plaidHeaders : List Http.Header
plaidHeaders =
    [ Http.header "Content-Type" "application/json"
    ]

-- Create Plaid Link Token for bank connection
createLinkToken : String -> (Result Http.Error LinkTokenResponse -> msg) -> Cmd msg
createLinkToken userId toMsg =
    let
        body = Encode.object
            [ ("client_id", Encode.string plaidClientId)
            , ("secret", Encode.string plaidSecret)
            , ("client_name", Encode.string "Financial Dashboard")
            , ("country_codes", Encode.list Encode.string ["US"])
            , ("language", Encode.string "en")
            , ("user", Encode.object [("client_user_id", Encode.string userId)])
            , ("products", Encode.list Encode.string ["transactions"])
            ]
    in
    Http.request
        { method = "POST"
        , headers = plaidHeaders
        , url = plaidBaseUrl ++ "/link/token/create"
        , body = Http.jsonBody body
        , expect = Http.expectJson toMsg linkTokenResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

-- Exchange public token for access token
exchangePublicToken : String -> (Result Http.Error AccessTokenResponse -> msg) -> Cmd msg
exchangePublicToken publicToken toMsg =
    let
        body = Encode.object
            [ ("client_id", Encode.string plaidClientId)
            , ("secret", Encode.string plaidSecret)
            , ("public_token", Encode.string publicToken)
            ]
    in
    Http.request
        { method = "POST"
        , headers = plaidHeaders
        , url = plaidBaseUrl ++ "/item/public_token/exchange"
        , body = Http.jsonBody body
        , expect = Http.expectJson toMsg accessTokenResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

-- Get accounts using access token
getAccounts : String -> (Result Http.Error AccountsResponse -> msg) -> Cmd msg
getAccounts accessToken toMsg =
    let
        body = Encode.object
            [ ("client_id", Encode.string plaidClientId)
            , ("secret", Encode.string plaidSecret)
            , ("access_token", Encode.string accessToken)
            ]
    in
    Http.request
        { method = "POST"
        , headers = plaidHeaders
        , url = plaidBaseUrl ++ "/accounts/get"
        , body = Http.jsonBody body
        , expect = Http.expectJson toMsg accountsResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

-- Get transactions
getTransactions : String -> String -> String -> (Result Http.Error TransactionsResponse -> msg) -> Cmd msg
getTransactions accessToken startDate endDate toMsg =
    let
        body = Encode.object
            [ ("client_id", Encode.string plaidClientId)
            , ("secret", Encode.string plaidSecret)
            , ("access_token", Encode.string accessToken)
            , ("start_date", Encode.string startDate)
            , ("end_date", Encode.string endDate)
            ]
    in
    Http.request
        { method = "POST"
        , headers = plaidHeaders
        , url = plaidBaseUrl ++ "/transactions/get"
        , body = Http.jsonBody body
        , expect = Http.expectJson toMsg transactionsResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

-- Response Types and Decoders

type alias LinkTokenResponse =
    { linkToken : String
    , expiration : String
    , requestId : String
    }

linkTokenResponseDecoder : Decoder LinkTokenResponse
linkTokenResponseDecoder =
    Decode.map3 LinkTokenResponse
        (Decode.field "link_token" Decode.string)
        (Decode.field "expiration" Decode.string)
        (Decode.field "request_id" Decode.string)

type alias AccessTokenResponse =
    { accessToken : String
    , itemId : String
    , requestId : String
    }

accessTokenResponseDecoder : Decoder AccessTokenResponse
accessTokenResponseDecoder =
    Decode.map3 AccessTokenResponse
        (Decode.field "access_token" Decode.string)
        (Decode.field "item_id" Decode.string)
        (Decode.field "request_id" Decode.string)

type alias AccountsResponse =
    { accounts : List PlaidAccount
    , requestId : String
    }

type alias PlaidAccount =
    { accountId : String
    , name : String
    , officialName : Maybe String
    , accountType : String
    , subtype : Maybe String
    , balances : PlaidBalance
    }

type alias PlaidBalance =
    { available : Maybe Float
    , current : Maybe Float
    , limit : Maybe Float
    , isoCurrencyCode : Maybe String
    }

accountsResponseDecoder : Decoder AccountsResponse
accountsResponseDecoder =
    Decode.map2 AccountsResponse
        (Decode.field "accounts" (Decode.list plaidAccountDecoder))
        (Decode.field "request_id" Decode.string)

plaidAccountDecoder : Decoder PlaidAccount
plaidAccountDecoder =
    Decode.map6 PlaidAccount
        (Decode.field "account_id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "official_name" (Decode.maybe Decode.string))
        (Decode.field "type" Decode.string)
        (Decode.field "subtype" (Decode.maybe Decode.string))
        (Decode.field "balances" plaidBalanceDecoder)

plaidBalanceDecoder : Decoder PlaidBalance
plaidBalanceDecoder =
    Decode.map4 PlaidBalance
        (Decode.field "available" (Decode.maybe Decode.float))
        (Decode.field "current" (Decode.maybe Decode.float))
        (Decode.field "limit" (Decode.maybe Decode.float))
        (Decode.field "iso_currency_code" (Decode.maybe Decode.string))

type alias TransactionsResponse =
    { accounts : List PlaidAccount
    , transactions : List PlaidTransaction
    , totalTransactions : Int
    , requestId : String
    }

type alias PlaidTransaction =
    { accountId : String
    , amount : Float
    , date : String
    , name : String
    , merchantName : Maybe String
    , category : List String
    , transactionId : String
    , pending : Bool
    }

transactionsResponseDecoder : Decoder TransactionsResponse
transactionsResponseDecoder =
    Decode.map4 TransactionsResponse
        (Decode.field "accounts" (Decode.list plaidAccountDecoder))
        (Decode.field "transactions" (Decode.list plaidTransactionDecoder))
        (Decode.field "total_transactions" Decode.int)
        (Decode.field "request_id" Decode.string)

plaidTransactionDecoder : Decoder PlaidTransaction
plaidTransactionDecoder =
    Decode.map8 PlaidTransaction
        (Decode.field "account_id" Decode.string)
        (Decode.field "amount" Decode.float)
        (Decode.field "date" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "merchant_name" (Decode.maybe Decode.string))
        (Decode.field "category" (Decode.list Decode.string))
        (Decode.field "transaction_id" Decode.string)
        (Decode.field "pending" Decode.bool)

-- Convert Plaid data to our internal models
plaidAccountToAccount : PlaidAccount -> Account
plaidAccountToAccount plaidAccount =
    { id = plaidAccount.accountId
    , name = plaidAccount.name
    , accountType = stringToAccountType plaidAccount.accountType
    , balance = Maybe.withDefault 0.0 plaidAccount.balances.current
    , availableBalance = plaidAccount.balances.available
    , currency = Maybe.withDefault "USD" plaidAccount.balances.isoCurrencyCode
    , lastUpdated = "2024-01-01"  -- This would come from the API
    , institutionName = "Connected Bank"
    }

plaidTransactionToTransaction : PlaidTransaction -> Transaction
plaidTransactionToTransaction plaidTransaction =
    { id = plaidTransaction.transactionId
    , accountId = plaidTransaction.accountId
    , amount = abs plaidTransaction.amount
    , description = plaidTransaction.name
    , date = plaidTransaction.date
    , category = String.join ", " plaidTransaction.category
    , transactionType = if plaidTransaction.amount > 0 then Transaction.Debit else Transaction.Credit
    , pending = plaidTransaction.pending
    , merchantName = plaidTransaction.merchantName
    }

stringToAccountType : String -> Account.AccountType
stringToAccountType typeString =
    case String.toLower typeString of
        "depository" -> Account.Checking
        "credit" -> Account.CreditCard
        "loan" -> Account.Loan
        "investment" -> Account.Investment
        _ -> Account.Checking
