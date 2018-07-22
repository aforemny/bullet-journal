module Type.DailySpread exposing (..)

import Time.Format.Locale as Calendar
import Type.Bullet as Bullet exposing (Bullet)


type alias DailySpread =
    { year : Year
    , month : Month
    , dayOfMonth : DayOfMonth
    , bullets : List Bullet
    }


type alias Month =
    Int


type alias Year =
    Int


type alias DayOfMonth =
    Int


empty : Year -> Month -> DayOfMonth -> DailySpread
empty year month dayOfMonth =
    { year = year
    , month = month
    , dayOfMonth = dayOfMonth
    , bullets = []
    }


canonicalDate : DailySpread -> ( Int, Int, Int )
canonicalDate dailySpread =
    ( dailySpread.year, dailySpread.month, dailySpread.dayOfMonth )


title : DailySpread -> String
title dailySpread =
    String.join " "
        [ toString dailySpread.dayOfMonth
        , case Calendar.defaultTimeLocale of
            Calendar.TimeLocale { months } ->
                List.drop (dailySpread.month - 1) months
                    |> List.head
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault ""
        , toString dailySpread.year
        ]
