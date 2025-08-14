module Utils.DateHelpers exposing (..)

import Time


-- Format a date string for display
formatDate : String -> String
formatDate dateString =
    -- This is a simplified implementation
    -- In a real application, you would use elm/time and parse the ISO date
    case String.split "-" dateString of
        [year, month, day] ->
            formatMonthName month ++ " " ++ day ++ ", " ++ year
        _ ->
            dateString

-- Format month number to month name
formatMonthName : String -> String
formatMonthName month =
    case month of
        "01" -> "Jan"
        "02" -> "Feb"
        "03" -> "Mar"
        "04" -> "Apr"
        "05" -> "May"
        "06" -> "Jun"
        "07" -> "Jul"
        "08" -> "Aug"
        "09" -> "Sep"
        "10" -> "Oct"
        "11" -> "Nov"
        "12" -> "Dec"
        _ -> month

-- Get relative date description
getRelativeDate : String -> String
getRelativeDate dateString =
    -- This is a simplified implementation
    -- In a real app, you would compare with current date
    "Recent"

-- Check if a date is within the last N days
isWithinDays : Int -> String -> Bool
isWithinDays days dateString =
    -- Simplified - always returns True for demo
    True

-- Format time duration
formatDuration : Int -> String
formatDuration seconds =
    if seconds < 60 then
        String.fromInt seconds ++ " seconds"
    else if seconds < 3600 then
        String.fromInt (seconds // 60) ++ " minutes"
    else
        String.fromInt (seconds // 3600) ++ " hours"

-- Get start of month date string
getStartOfMonth : String -> String
getStartOfMonth dateString =
    case String.split "-" dateString of
        [year, month, _] ->
            year ++ "-" ++ month ++ "-01"
        _ ->
            dateString

-- Get end of month date string  
getEndOfMonth : String -> String
getEndOfMonth dateString =
    case String.split "-" dateString of
        [year, month, _] ->
            let
                daysInMonth = case month of
                    "02" -> if isLeapYear (Maybe.withDefault 0 (String.toInt year)) then "29" else "28"
                    "04" -> "30"
                    "06" -> "30" 
                    "09" -> "30"
                    "11" -> "30"
                    _ -> "31"
            in
            year ++ "-" ++ month ++ "-" ++ daysInMonth
        _ ->
            dateString

-- Check if year is leap year
isLeapYear : Int -> Bool
isLeapYear year =
    (modBy 4 year == 0) && ((modBy 100 year /= 0) || (modBy 400 year == 0))

-- Parse ISO date string to components
parseIsoDate : String -> Maybe { year : Int, month : Int, day : Int }
parseIsoDate dateString =
    case String.split "-" dateString of
        [yearStr, monthStr, dayStr] ->
            Maybe.map3 (\y m d -> { year = y, month = m, day = d })
                (String.toInt yearStr)
                (String.toInt monthStr)
                (String.toInt (String.left 2 dayStr))  -- Handle time portion
        _ ->
            Nothing

-- Generate date range for API calls
getDateRange : Int -> { startDate : String, endDate : String }
getDateRange daysBack =
    -- Simplified implementation - in real app would use current date
    { startDate = "2024-01-01"
    , endDate = "2024-01-31" 
    }
