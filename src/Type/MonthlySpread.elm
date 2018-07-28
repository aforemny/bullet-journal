module Type.MonthlySpread exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Parse
import Parse.Decode
import Task exposing (Task)
import Time.Format.Locale as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Type.Day as Day exposing (Day)


type alias MonthlySpread =
    { year : Year
    , month : Month
    }


encode : MonthlySpread -> Value
encode monthlySpread =
    Encode.object
        [ ( "year", Encode.int monthlySpread.year )
        , ( "month", Encode.int monthlySpread.month )
        ]


decode : Decoder (Parse.Object MonthlySpread)
decode =
    Decode.decode
        (\objectId createdAt updatedAt year month ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , year = year
            , month = month
            }
        )
        |> Decode.required "objectId" Parse.Decode.objectId
        |> Decode.required "createdAt" Parse.Decode.date
        |> Decode.required "updatedAt" Parse.Decode.date
        |> Decode.required "year" Decode.int
        |> Decode.required "month" Decode.int


fromParseObject : Parse.Object MonthlySpread -> MonthlySpread
fromParseObject monthlySpread =
    { year = monthlySpread.year
    , month = monthlySpread.month
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


get :
    Parse.Config
    -> Parse.ObjectId MonthlySpread
    -> Task Parse.Error (Parse.Object MonthlySpread)
get parse objectId =
    Parse.toTask parse (Parse.get "MonthlySpread" decode objectId)



getBy :
    Parse.Config
    -> Year
    -> Month
    -> Task Parse.Error (Maybe (Parse.Object MonthlySpread))
getBy parse year month =
    Parse.toTask parse
        (Parse.query decode
            (Parse.emptyQuery "MonthlySpread"
                |> \query ->
                    { query
                        | whereClause =
                            Parse.and
                                [ Parse.equalTo "year" (Encode.int year)
                                , Parse.equalTo "month" (Encode.int month)
                                ]
                        , limit = Just 1
                    }
            )
        )
        |> Task.map List.head



create : Parse.Config -> MonthlySpread -> Task Parse.Error (Parse.ObjectId MonthlySpread)
create parse monthlySpread =
    let
        getExistingMonthlySpread =
            Task.map List.head <|
                Parse.toTask parse <|
                    Parse.query decode monthlySpreadQuery

        createNewMonthlySpread =
            Parse.toTask parse <|
                Parse.create "MonthlySpread" encode monthlySpread

        monthlySpreadQuery =
            Parse.emptyQuery "MonthlySpread"
                |> \query ->
                    { query
                        | whereClause =
                            Parse.and
                                [ Parse.equalTo "year" (Encode.int monthlySpread.year)
                                , Parse.equalTo "month" (Encode.int monthlySpread.month)
                                ]
                        , limit = Just 1
                    }
    in
        getExistingMonthlySpread
            |> Task.andThen
                (\existingMonthlySpread ->
                    case existingMonthlySpread of
                        Just monthlySpread ->
                            Task.succeed monthlySpread.objectId

                        Nothing ->
                            createNewMonthlySpread
                                |> Task.map .objectId
                )
