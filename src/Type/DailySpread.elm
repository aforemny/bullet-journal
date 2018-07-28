module Type.DailySpread exposing (..)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Parse
import Parse.Decode
import Task exposing (Task)
import Time.Format.Locale as Calendar
import Type.Bullet as Bullet exposing (Bullet)


type alias DailySpread =
    { year : Year
    , month : Month
    , dayOfMonth : DayOfMonth
    }


encode : DailySpread -> Value
encode dailySpread =
    Encode.object
        [ ( "year", Encode.int dailySpread.year )
        , ( "month", Encode.int dailySpread.month )
        , ( "dayOfMonth", Encode.int dailySpread.dayOfMonth )
        ]


decode : Decoder (Parse.Object DailySpread)
decode =
    Decode.decode
        (\objectId createdAt updatedAt year month dayOfMonth ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , year = year
            , month = month
            , dayOfMonth = dayOfMonth
            }
        )
        |> Decode.required "objectId" Parse.Decode.objectId
        |> Decode.required "createdAt" Parse.Decode.date
        |> Decode.required "updatedAt" Parse.Decode.date
        |> Decode.required "year" Decode.int
        |> Decode.required "month" Decode.int
        |> Decode.required "dayOfMonth" Decode.int


fromParseObject : Parse.Object DailySpread -> DailySpread
fromParseObject dailySpread =
    { year = dailySpread.year
    , month = dailySpread.month
    , dayOfMonth = dailySpread.dayOfMonth
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


get :
    Parse.Config
    -> Parse.ObjectId DailySpread
    -> Task Parse.Error (Parse.Object DailySpread)
get parse objectId =
    Parse.toTask parse (Parse.get "DailySpread" decode objectId)


getBy :
    Parse.Config
    -> Year
    -> Month
    -> DayOfMonth
    -> Task Parse.Error (Maybe (Parse.Object DailySpread))
getBy parse year month dayOfMonth =
    Parse.toTask parse
        (Parse.query decode
            (Parse.emptyQuery "DailySpread"
                |> \query ->
                    { query
                        | whereClause =
                            Parse.and
                                [ Parse.equalTo "year" (Encode.int year)
                                , Parse.equalTo "month" (Encode.int month)
                                , Parse.equalTo "dayOfMonth" (Encode.int dayOfMonth)
                                ]
                        , limit = Just 1
                    }
            )
        )
        |> Task.map List.head


create : Parse.Config -> DailySpread -> Task Parse.Error (Parse.ObjectId DailySpread)
create parse dailySpread =
    let
        getExistingDailySpread =
            Task.map List.head <|
                Parse.toTask parse <|
                    Parse.query decode dailySpreadQuery

        createNewDailySpread =
            Parse.toTask parse <|
                Parse.create "DailySpread" encode dailySpread

        dailySpreadQuery =
            Parse.emptyQuery "DailySpread"
                |> \query ->
                    { query
                        | whereClause =
                            Parse.and
                                [ Parse.equalTo "year" (Encode.int dailySpread.year)
                                , Parse.equalTo "month" (Encode.int dailySpread.month)
                                , Parse.equalTo "dayOfMonth"
                                    (Encode.int dailySpread.dayOfMonth)
                                ]
                        , limit = Just 1
                    }
    in
        getExistingDailySpread
            |> Task.andThen
                (\existingDailySpread ->
                    case existingDailySpread of
                        Just dailySpread ->
                            Task.succeed dailySpread.objectId

                        Nothing ->
                            createNewDailySpread
                                |> Task.map .objectId
                )
