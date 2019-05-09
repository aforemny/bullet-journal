module Type.Day exposing (Create, Day, DayOfMonth, Month, Update, create, createOrUpdate, decode, encode, fromParseObject, get, list, update)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Parse
import Parse.Decode
import Task exposing (Task)
import Time


type alias Day =
    { month : Month
    , dayOfMonth : DayOfMonth
    , text : String
    }


encode : Day -> Value
encode day =
    Encode.object
        [ ( "month", Encode.int day.month )
        , ( "dayOfMonth", Encode.int day.dayOfMonth )
        , ( "text", Encode.string day.text )
        ]


decode : Decoder (Parse.Object Day)
decode =
    Decode.succeed
        (\objectId createdAt updatedAt month dayOfMonth text ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , month = month
            , dayOfMonth = dayOfMonth
            , text = text
            }
        )
        |> Decode.required "objectId" Parse.Decode.objectId
        |> Decode.required "createdAt" Parse.Decode.date
        |> Decode.required "updatedAt" Parse.Decode.date
        |> Decode.required "month" Decode.int
        |> Decode.required "dayOfMonth" Decode.int
        |> Decode.required "text" Decode.string


type alias Month =
    Int


type alias DayOfMonth =
    Int


fromParseObject : Parse.Object Day -> Day
fromParseObject day =
    { month = day.month
    , dayOfMonth = day.dayOfMonth
    , text = day.text
    }


get : Parse.Config -> Month -> DayOfMonth -> Task Parse.Error (Maybe (Parse.Object Day))
get parse month dayOfMonth =
    Parse.toTask parse
        (Parse.query decode
            (Parse.emptyQuery "Day"
                |> (\query ->
                        { query
                            | whereClause =
                                Parse.and
                                    [ Parse.equalTo "month" (Encode.int month)
                                    , Parse.equalTo "dayOfMonth" (Encode.int dayOfMonth)
                                    ]
                            , limit = Just 1
                        }
                   )
            )
        )
        |> Task.map List.head


list : Parse.Config -> Month -> Task Parse.Error (List (Parse.Object Day))
list parse month =
    Parse.toTask parse
        (Parse.query decode
            (Parse.emptyQuery "Day"
                |> (\query ->
                        { query
                            | whereClause =
                                Parse.and
                                    [ Parse.equalTo "month" (Encode.int month)
                                    ]
                        }
                   )
            )
        )


type alias Create =
    { createdAt : Time.Posix
    , objectId : Parse.ObjectId Day
    }


type alias Update =
    { updatedAt : Time.Posix
    }


createOrUpdate :
    Parse.Config
    -> Day
    -> Task Parse.Error (Result Update Create)
createOrUpdate parse day =
    get parse day.month day.dayOfMonth
        |> Task.andThen
            (\maybeOtherDay ->
                case maybeOtherDay of
                    Just otherDay ->
                        update parse
                            { otherDay
                                | month = day.month
                                , dayOfMonth = day.dayOfMonth
                                , text = day.text
                            }
                            |> Task.map Err

                    Nothing ->
                        create parse day
                            |> Task.map Ok
            )


create : Parse.Config -> Day -> Task Parse.Error Create
create parse day =
    Parse.toTask parse (Parse.create "Day" encode day)


update : Parse.Config -> Parse.Object Day -> Task Parse.Error Update
update parse day =
    Parse.toTask parse (Parse.update "Day" encode day.objectId (fromParseObject day))
