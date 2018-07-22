module State exposing (..)

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


type alias State =
    { monthlySpreads : List MonthlySpread
    , dailySpreads : List DailySpread
    , collectionSpreads : List CollectionSpread
    }


type alias MonthlySpread =
    { year : Int
    , month : Int
    , items : List { dayOfMonth : Int, text : String }
    , bullets : List String
    }


encodeMonthlySpread : MonthlySpread -> Value
encodeMonthlySpread monthlySpread =
    let
        encodeItem item =
            Encode.object
                [ ( "dayOfMonth", Encode.int item.dayOfMonth )
                , ( "text", Encode.string item.text )
                ]
    in
        Encode.object
            [ ( "year", Encode.int monthlySpread.year )
            , ( "month", Encode.int monthlySpread.month )
            , ( "items", Encode.list (List.map encodeItem monthlySpread.items) )
            , ( "bullets", Encode.list (List.map Encode.string monthlySpread.bullets) )
            ]


decodeMonthlySpread : Decoder MonthlySpread
decodeMonthlySpread =
    let
        decodeItem =
            Decode.map2
                (\dayOfMonth text ->
                    { dayOfMonth = dayOfMonth, text = text }
                )
                (Decode.at [ "dayOfMonth" ] Decode.int)
                (Decode.at [ "text" ] Decode.string)
    in
        Decode.map4 MonthlySpread
            (Decode.at [ "year" ] Decode.int)
            (Decode.at [ "month" ] Decode.int)
            (Decode.at [ "items" ] (Decode.list decodeItem))
            (Decode.at [ "bullets" ] (Decode.list Decode.string))


type alias DailySpread =
    { year : Int
    , month : Int
    , dayOfMonth : Int
    , bullets : List String
    }


encodeDailySpread : DailySpread -> Value
encodeDailySpread dailySpread =
    Encode.object
        [ ( "year", Encode.int dailySpread.year )
        , ( "month", Encode.int dailySpread.month )
        , ( "dayOfMonth", Encode.int dailySpread.dayOfMonth )
        , ( "bullets", Encode.list (List.map Encode.string dailySpread.bullets) )
        ]


decodeDailySpread : Decoder DailySpread
decodeDailySpread =
    Decode.map4 DailySpread
        (Decode.at [ "year" ] Decode.int)
        (Decode.at [ "month" ] Decode.int)
        (Decode.at [ "dayOfMonth" ] Decode.int)
        (Decode.at [ "bullets" ] (Decode.list Decode.string))


type alias CollectionSpread =
    { id : String
    , createdDate : Date
    , title : String
    , bullets : List String
    }


encodeCollectionSpread : CollectionSpread -> Value
encodeCollectionSpread collectionSpread =
    Encode.object
        [ ( "id", Encode.string collectionSpread.id )
        , ( "createdDate", encodeDate collectionSpread.createdDate )
        , ( "title", Encode.string collectionSpread.title )
        , ( "bullets", Encode.list (List.map Encode.string collectionSpread.bullets) )
        ]


decodeCollectionSpread : Decoder CollectionSpread
decodeCollectionSpread =
    Decode.map4 CollectionSpread
        (Decode.at [ "id" ] Decode.string)
        (Decode.at [ "createdDate" ] decodeDate)
        (Decode.at [ "title" ] Decode.string)
        (Decode.at [ "bullets" ] (Decode.list Decode.string))


encode : State -> Value
encode state =
    Encode.object
        [ ( "monthlySpreads", Encode.list (List.map encodeMonthlySpread state.monthlySpreads) )
        , ( "dailySpreads", Encode.list (List.map encodeDailySpread state.dailySpreads) )
        , ( "collectionSpreads", Encode.list (List.map encodeCollectionSpread state.collectionSpreads) )
        ]


decode : Decoder State
decode =
    Decode.map3 State
        (Decode.at [ "monthlySpreads" ] (Decode.list decodeMonthlySpread))
        (Decode.at [ "dailySpreads" ] (Decode.list decodeDailySpread))
        (Decode.at [ "collectionSpreads" ] (Decode.list decodeCollectionSpread))


encodeDate : Date -> Value
encodeDate date =
    Encode.float (Date.toTime date)


decodeDate : Decoder Date
decodeDate =
    Decode.map Date.fromTime Decode.float
