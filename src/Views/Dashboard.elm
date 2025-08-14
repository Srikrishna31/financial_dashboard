module Views.Dashboard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models.Account as Account exposing (Account)
import Models.Transaction as Transaction exposing (Transaction)
import Models.CreditCard as CreditCard exposing (CreditCard)
import Views.AccountSummary as AccountSummary
import Views.TransactionList as TransactionList


type alias DashboardData =
    { accounts : List Account
    , transactions : List Transaction
    , creditCards : List CreditCard
    , selectedAccount : Maybe Account
    , loading : Bool
    }

view : DashboardData -> (Account -> msg) -> Html msg
view data onSelectAccount =
    div [ class "dashboard-container" ]
        [ div [ class "dashboard-sidebar" ]
            [ accountsSection data.accounts data.selectedAccount onSelectAccount
            , creditCardsSection data.creditCards
            ]
        , div [ class "dashboard-content" ]
            [ overviewSection data
            , transactionsSection data.transactions data.loading
            ]
        ]

accountsSection : List Account -> Maybe Account -> (Account -> msg) -> Html msg
accountsSection accounts selectedAccount onSelectAccount =
    section [ class "accounts-section" ]
        [ h2 [] [ text "Accounts" ]
        , div [ class "accounts-list" ]
            (List.map (accountItem selectedAccount onSelectAccount) accounts)
        ]

accountItem : Maybe Account -> (Account -> msg) -> Account -> Html msg
accountItem selectedAccount onSelectAccount account =
    let
        isSelected = case selectedAccount of
            Just selected -> selected.id == account.id
            Nothing -> False

        itemClass = if isSelected then "account-item selected" else "account-item"
    in
    div 
        [ class itemClass
        , onClick (onSelectAccount account)
        ]
        [ div [ class "account-info" ]
            [ div [ class "account-name" ] [ text account.name ]
            , div [ class "account-type" ] 
                [ text (Account.accountTypeToString account.accountType) ]
            , div [ class "account-institution" ] [ text account.institutionName ]
            ]
        , div [ class "account-balance" ]
            [ text (Account.formatBalance account) ]
        ]

creditCardsSection : List CreditCard -> Html msg
creditCardsSection creditCards =
    section [ class "credit-cards-section" ]
        [ h2 [] [ text "Credit Cards" ]
        , div [ class "credit-cards-list" ]
            (List.map creditCardItem creditCards)
        ]

creditCardItem : CreditCard -> Html msg
creditCardItem card =
    div [ class "credit-card-item" ]
        [ div [ class "card-header" ]
            [ div [ class "card-type" ] 
                [ text (CreditCard.cardTypeToString card.cardType) ]
            , div [ class "card-number" ] 
                [ text (CreditCard.maskCardNumber card.cardNumber) ]
            ]
        , div [ class "card-details" ]
            [ div [ class "balance-info" ]
                [ div [] [ text "Balance: ", text (CreditCard.formatCurrency card.currentBalance) ]
                , div [] [ text "Available: ", text (CreditCard.formatCurrency card.availableCredit) ]
                ]
            , div [ class "utilization" ]
                [ div [ class "utilization-bar" ]
                    [ div 
                        [ class "utilization-fill"
                        , style "width" (String.fromFloat (CreditCard.utilizationPercentage card) ++ "%")
                        ]
                        []
                    ]
                , div [ class "utilization-text" ]
                    [ text (String.fromFloat (CreditCard.utilizationPercentage card) ++ "% used") ]
                ]
            , if card.paymentDueWarning then
                div [ class "payment-warning" ]
                    [ text ("Payment due: " ++ card.dueDate) ]
              else
                text ""
            ]
        ]

overviewSection : DashboardData -> Html msg
overviewSection data =
    section [ class "overview-section" ]
        [ h2 [] [ text "Account Overview" ]
        , case data.selectedAccount of
            Just account ->
                AccountSummary.view account data.transactions
            Nothing ->
                div [ class "no-selection" ]
                    [ text "Select an account to view details" ]
        ]

transactionsSection : List Transaction -> Bool -> Html msg
transactionsSection transactions loading =
    section [ class "transactions-section" ]
        [ h2 [] [ text "Recent Transactions" ]
        , if loading then
            div [ class "loading" ] [ text "Loading transactions..." ]
          else
            TransactionList.view transactions
        ]
