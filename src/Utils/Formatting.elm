module Utils.Formatting exposing (..)

import Utils.DateHelpers as DateHelpers


-- Format currency amounts
formatCurrency : Float -> String
formatCurrency amount =
    "$" ++ formatNumber amount

-- Format numbers with commas
formatNumber : Float -> String
formatNumber number =
    let
        rounded = String.fromFloat (toFloat (round (number * 100)) / 100)
        parts = String.split "." rounded
        wholePart = Maybe.withDefault "0" (List.head parts)
        decimalPart = Maybe.withDefault "00" (List.head (List.drop 1 parts))

        -- Pad decimal part to 2 digits
        paddedDecimal = String.padRight 2 '0' decimalPart
    in
    addCommas wholePart ++ "." ++ paddedDecimal

-- Add commas to large numbers
addCommas : String -> String
addCommas numberString =
    let
        reversed = String.reverse numberString
        withCommas = addCommasHelper reversed 0 ""
    in
    String.reverse withCommas

addCommasHelper : String -> Int -> String -> String
addCommasHelper remaining count result =
    case String.uncons remaining of
        Nothing ->
            result
        Just (char, rest) ->
            let
                newResult = 
                    if count > 0 && modBy 3 count == 0 then
                        String.cons char ("," ++ result)
                    else
                        String.cons char result
            in
            addCommasHelper rest (count + 1) newResult

-- Format percentage
formatPercentage : Float -> String
formatPercentage percentage =
    String.fromFloat (toFloat (round (percentage * 10)) / 10) ++ "%"

-- Format account balance with appropriate styling info
formatAccountBalance : Float -> { formatted : String, isNegative : Bool }
formatAccountBalance balance =
    { formatted = formatCurrency (abs balance)
    , isNegative = balance < 0
    }

-- Format transaction amount with sign
formatTransactionAmount : Float -> Bool -> String
formatTransactionAmount amount isDebit =
    let
        sign = if isDebit then "-" else "+"
        formatted = formatCurrency (abs amount)
    in
    sign ++ formatted

-- Format date for display
formatDate : String -> String
formatDate dateString =
    DateHelpers.formatDate dateString

-- Format relative date
formatRelativeDate : String -> String
formatRelativeDate dateString =
    DateHelpers.getRelativeDate dateString

-- Format credit utilization
formatUtilization : Float -> Float -> String
formatUtilization used limit =
    if limit > 0 then
        let
            percentage = (used / limit) * 100
        in
        formatPercentage percentage
    else
        "0%"

-- Format APR
formatApr : Float -> String
formatApr apr =
    String.fromFloat apr ++ "%"

-- Format card number (mask all but last 4)
formatCardNumber : String -> String
formatCardNumber cardNumber =
    let
        length = String.length cardNumber
        lastFour = String.right 4 cardNumber
    in
    if length <= 4 then
        cardNumber
    else
        String.repeat (length - 4) "*" ++ lastFour

-- Format account type for display
formatAccountType : String -> String
formatAccountType accountType =
    case String.toLower accountType of
        "checking" -> "Checking Account"
        "savings" -> "Savings Account" 
        "credit_card" -> "Credit Card"
        "investment" -> "Investment Account"
        "loan" -> "Loan Account"
        _ -> String.toUpper (String.left 1 accountType) ++ String.dropLeft 1 accountType

-- Format institution name
formatInstitutionName : String -> String
formatInstitutionName name =
    if String.isEmpty name then
        "Unknown Bank"
    else
        name

-- Truncate long text with ellipsis
truncateText : Int -> String -> String
truncateText maxLength text =
    if String.length text <= maxLength then
        text
    else
        String.left (maxLength - 3) text ++ "..."

-- Format file size
formatFileSize : Int -> String
formatFileSize bytes =
    let
        kb = toFloat bytes / 1024
        mb = kb / 1024
        gb = mb / 1024
    in
    if gb >= 1 then
        String.fromFloat (toFloat (round (gb * 10)) / 10) ++ " GB"
    else if mb >= 1 then
        String.fromFloat (toFloat (round (mb * 10)) / 10) ++ " MB" 
    else if kb >= 1 then
        String.fromFloat (toFloat (round (kb * 10)) / 10) ++ " KB"
    else
        String.fromInt bytes ++ " bytes"

-- Format loading states
formatLoadingMessage : String -> String
formatLoadingMessage operation =
    "Loading " ++ operation ++ "..."

-- Format error messages for user display
formatErrorMessage : String -> String
formatErrorMessage error =
    "Error: " ++ error
