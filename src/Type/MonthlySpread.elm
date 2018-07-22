module Type.MonthlySpread exposing (..)

import Dict exposing (Dict)
import Time.Format.Locale as Calendar
import Type.Bullet as Bullet exposing (Bullet)


type alias MonthlySpread =
    { year : Year
    , month : Month
    , items : Dict DayOfMonth String
    , bullets : List Bullet
    }


type alias Month =
    Int


type alias Year =
    Int


type alias DayOfMonth =
    Int


empty : Year -> Month -> MonthlySpread
empty year month =
    { year = year
    , month = month
    , items = Dict.empty
    , bullets = []
    }


canonicalDate : MonthlySpread -> ( Int, Int, Int )
canonicalDate model =
    ( model.year, model.month, 0 )


title : MonthlySpread -> String
title monthlySpread =
    String.join " "
        [ case Calendar.defaultTimeLocale of
            Calendar.TimeLocale { months } ->
                List.drop (monthlySpread.month - 1) months
                    |> List.head
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault ""
        , toString monthlySpread.year
        ]
