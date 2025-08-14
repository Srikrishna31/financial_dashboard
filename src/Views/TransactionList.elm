module Views.TransactionList exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models.Transaction as Transaction exposing (Transaction)
import Utils.Formatting as Formatting


view : List Transaction -> Html msg
view transactions =
    if List.isEmpty transactions then
        div [ class "no-transactions" ]
            [ text "No transactions found" ]
    else
        div [ class "transaction-list" ]
            [ transactionTable transactions ]

transactionTable : List Transaction -> Html msg
transactionTable transactions =
    table [ class "transactions-table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Date" ]
                , th [] [ text "Description" ]
                , th [] [ text "Category" ]
                , th [] [ text "Amount" ]
                , th [] [ text "Status" ]
                ]
            ]
        , tbody []
            (List.map transactionRow transactions)
        ]

transactionRow : Transaction -> Html msg
transactionRow transaction =
    tr [ class "transaction-row" ]
        [ td [ class "transaction-date" ]
            [ text (Formatting.formatDate transaction.date) ]
        , td [ class "transaction-description" ]
            [ div []
                [ div [ class "primary-description" ] 
                    [ text transaction.description ]
                , case transaction.merchantName of
                    Just merchant ->
                        div [ class "merchant-name" ] 
                            [ text merchant ]
                    Nothing ->
                        text ""
                ]
            ]
        , td [ class "transaction-category" ]
            [ span [ class "category-badge" ]
                [ text (Transaction.categorizeTransaction transaction) ]
            ]
        , td [ class ("transaction-amount " ++ 
            (case transaction.transactionType of
                Transaction.Debit -> "debit"
                Transaction.Credit -> "credit"
            )) ]
            [ text (Transaction.formatAmount transaction) ]
        , td [ class "transaction-status" ]
            [ if transaction.pending then
                span [ class "status-badge pending" ] [ text "Pending" ]
              else
                span [ class "status-badge completed" ] [ text "Completed" ]
            ]
        ]

transactionSummary : List Transaction -> Html msg
transactionSummary transactions =
    let
        totalDebits = transactions
            |> List.filter (\t -> t.transactionType == Transaction.Debit)
            |> List.map .amount
            |> List.sum

        totalCredits = transactions
            |> List.filter (\t -> t.transactionType == Transaction.Credit)
            |> List.map .amount
            |> List.sum

        netAmount = totalCredits - totalDebits
    in
    div [ class "transaction-summary" ]
        [ div [ class "summary-item" ]
            [ span [ class "summary-label" ] [ text "Total Spent: " ]
            , span [ class "summary-value debit" ] [ text ("$" ++ String.fromFloat totalDebits) ]
            ]
        , div [ class "summary-item" ]
            [ span [ class "summary-label" ] [ text "Total Received: " ]
            , span [ class "summary-value credit" ] [ text ("$" ++ String.fromFloat totalCredits) ]
            ]
        , div [ class "summary-item" ]
            [ span [ class "summary-label" ] [ text "Net: " ]
            , span [ class ("summary-value " ++ (if netAmount >= 0 then "credit" else "debit")) ]
                [ text (if netAmount >= 0 then "+" else "", "$", String.fromFloat (abs netAmount)) ]
            ]
        ]
