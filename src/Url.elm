module Url exposing (..)

import Navigation
import String
import UrlParser exposing (..)
import View.CollectionSpread as CollectionSpread
import View.DailySpread as DailySpread
import View.MonthlySpread as MonthlySpread


type Url
    = Index
    | MonthlySpread MonthlySpread.Year MonthlySpread.Month
    | DailySpread DailySpread.Year DailySpread.Month DailySpread.DayOfMonth
    | CollectionSpread CollectionSpread.Id
    | NotFound String


toString : Url -> String
toString url =
    String.cons '#' <|
        case url of
            Index ->
                ""

            MonthlySpread year month ->
                String.join "/"
                    [ "monthly-spread"
                    , Basics.toString year
                    , Basics.toString month
                    ]

            DailySpread year month dayOfMonth ->
                String.join "/"
                    [ "daily-spread"
                    , Basics.toString year
                    , Basics.toString month
                    , Basics.toString dayOfMonth
                    ]

            CollectionSpread id ->
                "collection-spread/" ++ id

            NotFound hash ->
                hash


fromLocation : Navigation.Location -> Url
fromLocation location =
    case
        parseHash parseUrl location
            |> Maybe.withDefault (NotFound (String.dropLeft 1 location.hash))
    of
        NotFound "" ->
            Index

        url ->
            url


parseUrl =
    oneOf
        [ map Index (s "")
        , map MonthlySpread (s "monthly-spread" </> int </> int)
        , map DailySpread (s "daily-spread" </> int </> int </> int)
        , map CollectionSpread (s "collection-spread" </> string)
        ]
