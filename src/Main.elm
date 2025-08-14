module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Models.Account as Account exposing (Account)
import Models.Transaction as Transaction exposing (Transaction)
import Models.CreditCard as CreditCard exposing (CreditCard)
import Views.Dashboard as Dashboard
import Api.BankConnection as BankApi


-- MODEL

type alias Model =
    { accounts : List Account
    , transactions : List Transaction
    , creditCards : List CreditCard
    , selectedAccount : Maybe Account
    , loading : Bool
    , error : Maybe String
    , bankConnectionStatus : BankConnectionStatus
    }

type BankConnectionStatus
    = NotConnected
    | Connecting
    | Connected
    | ConnectionError String

init : () -> (Model, Cmd Msg)
init _ =
    ( { accounts = []
      , transactions = []
      , creditCards = []
      , selectedAccount = Nothing
      , loading = False
      , error = Nothing
      , bankConnectionStatus = NotConnected
      }
    , Cmd.none
    )


-- UPDATE

type Msg
    = ConnectToBank
    | BankConnectionResult (Result Http.Error (List Account))
    | LoadTransactions String
    | TransactionsLoaded (Result Http.Error (List Transaction))
    | LoadCreditCards
    | CreditCardsLoaded (Result Http.Error (List CreditCard))
    | SelectAccount Account
    | ClearError

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ConnectToBank ->
            ( { model | bankConnectionStatus = Connecting }
            , BankApi.connectToBank BankConnectionResult
            )

        BankConnectionResult (Ok accounts) ->
            ( { model 
                | accounts = accounts
                , bankConnectionStatus = Connected
                , loading = False
              }
            , Cmd.batch
                [ BankApi.loadAllCreditCards CreditCardsLoaded
                ]
            )

        BankConnectionResult (Err error) ->
            ( { model 
                | bankConnectionStatus = ConnectionError (BankApi.errorToString error)
                , loading = False
              }
            , Cmd.none
            )

        LoadTransactions accountId ->
            ( { model | loading = True }
            , BankApi.loadTransactions accountId TransactionsLoaded
            )

        TransactionsLoaded (Ok transactions) ->
            ( { model 
                | transactions = transactions
                , loading = False
              }
            , Cmd.none
            )

        TransactionsLoaded (Err error) ->
            ( { model 
                | error = Just (BankApi.errorToString error)
                , loading = False
              }
            , Cmd.none
            )

        LoadCreditCards ->
            ( { model | loading = True }
            , BankApi.loadAllCreditCards CreditCardsLoaded
            )

        CreditCardsLoaded (Ok creditCards) ->
            ( { model 
                | creditCards = creditCards
                , loading = False
              }
            , Cmd.none
            )

        CreditCardsLoaded (Err error) ->
            ( { model 
                | error = Just (BankApi.errorToString error)
                , loading = False
              }
            , Cmd.none
            )

        SelectAccount account ->
            ( { model | selectedAccount = Just account }
            , BankApi.loadTransactions account.id TransactionsLoaded
            )

        ClearError ->
            ( { model | error = Nothing }
            , Cmd.none
            )


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "financial-dashboard" ]
        [ header [ class "dashboard-header" ]
            [ h1 [] [ text "Financial Dashboard" ]
            , connectionStatus model.bankConnectionStatus
            ]
        , case model.error of
            Just error ->
                div [ class "error-banner" ]
                    [ text error
                    , button [ onClick ClearError ] [ text "×" ]
                    ]
            Nothing ->
                text ""
        , main_ [ class "dashboard-main" ]
            [ Dashboard.view 
                { accounts = model.accounts
                , transactions = model.transactions
                , creditCards = model.creditCards
                , selectedAccount = model.selectedAccount
                , loading = model.loading
                }
                SelectAccount
            ]
        ]

connectionStatus : BankConnectionStatus -> Html Msg
connectionStatus status =
    case status of
        NotConnected ->
            button [ onClick ConnectToBank, class "connect-btn" ]
                [ text "Connect to Bank" ]

        Connecting ->
            div [ class "status connecting" ]
                [ text "Connecting to bank..." ]

        Connected ->
            div [ class "status connected" ]
                [ text "✓ Connected to bank" ]

        ConnectionError error ->
            div [ class "status error" ]
                [ text ("Connection Error: " ++ error) ]


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
