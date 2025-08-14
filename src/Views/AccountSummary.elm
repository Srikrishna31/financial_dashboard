module Views.AccountSummary exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models.Account as Account exposing (Account)
import Models.Transaction as Transaction exposing (Transaction)
import Utils.Formatting as Formatting


view : Account -> List Transaction -> Html msg
view account transactions =
    div [ class "account-summary" ]
        [ accountHeader account
        , accountStats account transactions
        , recentActivity transactions
        ]

accountHeader : Account -> Html msg
accountHeader account =
    div [ class "account-header" ]
        [ div [ class "account-title" ]
            [ h3 [] [ text account.name ]
            , span [ class "account-type-badge" ]
                [ text (Account.accountTypeToString account.accountType) ]
            ]
        , div [ class "account-balance-large" ]
            [ text (Account.formatBalance account) ]
        , div [ class "account-meta" ]
            [ div [] [ text ("Institution: " ++ account.institutionName) ]
            , div [] [ text ("Last Updated: " ++ Formatting.formatDate account.lastUpdated) ]
            ]
        ]

accountStats : Account -> List Transaction -> Html msg
accountStats account transactions =
    let
        accountTransactions = List.filter (\t -> t.accountId == account.id) transactions
        totalSpent = accountTransactions
            |> List.filter (\t -> t.transactionType == Transaction.Debit)
            |> List.map .amount
            |> List.sum
        totalReceived = accountTransactions
            |> List.filter (\t -> t.transactionType == Transaction.Credit)
            |> List.map .amount
            |> List.sum
        transactionCount = List.length accountTransactions
    in
    div [ class "account-stats" ]
        [ div [ class "stat-card" ]
            [ div [ class "stat-label" ] [ text "Total Spent" ]
            , div [ class "stat-value debit" ] [ text ("$" ++ String.fromFloat totalSpent) ]
            ]
        , div [ class "stat-card" ]
            [ div [ class "stat-label" ] [ text "Total Received" ]
            , div [ class "stat-value credit" ] [ text ("$" ++ String.fromFloat totalReceived) ]
            ]
        , div [ class "stat-card" ]
            [ div [ class "stat-label" ] [ text "Transactions" ]
            , div [ class "stat-value" ] [ text (String.fromInt transactionCount) ]
            ]
        , case account.availableBalance of
            Just available ->
                div [ class "stat-card" ]
                    [ div [ class "stat-label" ] [ text "Available" ]
                    , div [ class "stat-value" ] [ text ("$" ++ String.fromFloat available) ]
                    ]
            Nothing ->
                text ""
        ]

recentActivity : List Transaction -> Html msg
recentActivity transactions =
    let
        recentTransactions = transactions
            |> List.filter Transaction.isRecent
            |> List.take 5
    in
    div [ class "recent-activity" ]
        [ h4 [] [ text "Recent Activity" ]
        , if List.isEmpty recentTransactions then
            div [ class "no-transactions" ]
                [ text "No recent transactions" ]
          else
            div [ class "activity-list" ]
                (List.map activityItem recentTransactions)
        ]

activityItem : Transaction -> Html msg
activityItem transaction =
    div [ class "activity-item" ]
        [ div [ class "activity-info" ]
            [ div [ class "activity-description" ] [ text transaction.description ]
            , div [ class "activity-category" ] 
                [ text (Transaction.categorizeTransaction transaction) ]
            , div [ class "activity-date" ] 
                [ text (Formatting.formatDate transaction.date) ]
            ]
        , div [ class ("activity-amount " ++ 
            (case transaction.transactionType of
                Transaction.Debit -> "debit"
                Transaction.Credit -> "credit"
            )) ]
            [ text (Transaction.formatAmount transaction) ]
        ]
