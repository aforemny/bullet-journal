module State exposing (..)

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Type.Bullet as Bullet exposing (Bullet)


type alias State =
    { monthlySpreads : List MonthlySpread
    , dailySpreads : List DailySpread
    , collectionSpreads : List CollectionSpread
    }


type alias MonthlySpread =
    { year : Int
    , month : Int
    , items : List { dayOfMonth : Int, text : String }
    , bullets : List Bullet
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
            , ( "bullets", Encode.list (List.map encodeBullet monthlySpread.bullets) )
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
            (Decode.at [ "bullets" ] (Decode.list decodeBullet))


type alias DailySpread =
    { year : Int
    , month : Int
    , dayOfMonth : Int
    , bullets : List Bullet
    }


encodeDailySpread : DailySpread -> Value
encodeDailySpread dailySpread =
    Encode.object
        [ ( "year", Encode.int dailySpread.year )
        , ( "month", Encode.int dailySpread.month )
        , ( "dayOfMonth", Encode.int dailySpread.dayOfMonth )
        , ( "bullets", Encode.list (List.map encodeBullet dailySpread.bullets) )
        ]


decodeDailySpread : Decoder DailySpread
decodeDailySpread =
    Decode.map4 DailySpread
        (Decode.at [ "year" ] Decode.int)
        (Decode.at [ "month" ] Decode.int)
        (Decode.at [ "dayOfMonth" ] Decode.int)
        (Decode.at [ "bullets" ] (Decode.list decodeBullet))


type alias CollectionSpread =
    { id : String
    , createdDate : Date
    , title : String
    , bullets : List Bullet
    }


encodeCollectionSpread : CollectionSpread -> Value
encodeCollectionSpread collectionSpread =
    Encode.object
        [ ( "id", Encode.string collectionSpread.id )
        , ( "createdDate", encodeDate collectionSpread.createdDate )
        , ( "title", Encode.string collectionSpread.title )
        , ( "bullets", Encode.list (List.map encodeBullet collectionSpread.bullets) )
        ]


decodeCollectionSpread : Decoder CollectionSpread
decodeCollectionSpread =
    Decode.map4 CollectionSpread
        (Decode.at [ "id" ] Decode.string)
        (Decode.at [ "createdDate" ] decodeDate)
        (Decode.at [ "title" ] Decode.string)
        (Decode.at [ "bullets" ] (Decode.list decodeBullet))


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


encodeBullet : Bullet -> Value
encodeBullet bullet =
    case bullet of
        Bullet.Task task ->
            Encode.object
                [ ( "ctor", Encode.string "task" )
                , ( "text", Encode.string task.text )
                , ( "state", encodeTaskState task.state )
                ]

        Bullet.Event event ->
            Encode.object
                [ ( "ctor", Encode.string "event" )
                , ( "text", Encode.string event.text )
                ]

        Bullet.Note note ->
            Encode.object
                [ ( "ctor", Encode.string "note" )
                , ( "text", Encode.string note.text )
                ]


decodeBullet : Decoder Bullet
decodeBullet =
    let
        decodeTask =
            Decode.map Bullet.Task <|
                Decode.map2
                    (\text state ->
                        { text = text, state = state }
                    )
                    (Decode.at [ "text" ] Decode.string)
                    (Decode.at [ "state" ] decodeTaskState)

        decodeEvent =
            Decode.map
                (\text ->
                    Bullet.Event { text = text }
                )
                (Decode.at [ "text" ] Decode.string)

        decodeNote =
            Decode.map
                (\text ->
                    Bullet.Note { text = text }
                )
                (Decode.at [ "text" ] Decode.string)
    in
        Decode.at [ "ctor" ] Decode.string
            |> Decode.andThen
                (\ctor ->
                    case ctor of
                        "task" ->
                            decodeTask

                        "event" ->
                            decodeEvent

                        "note" ->
                            decodeNote

                        _ ->
                            Decode.fail ("expected task, event or note but got " ++ toString ctor)
                )


encodeTaskState : Bullet.TaskState -> Value
encodeTaskState taskState =
    case taskState of
        Bullet.Unchecked ->
            Encode.string "unchecked"

        Bullet.Checked ->
            Encode.string "checked"

        Bullet.Migrated ->
            Encode.string "migrated"


decodeTaskState : Decoder Bullet.TaskState
decodeTaskState =
    Decode.string
        |> Decode.andThen
            (\taskStateString ->
                case taskStateString of
                    "unchecked" ->
                        Decode.succeed Bullet.Unchecked

                    "checked" ->
                        Decode.succeed Bullet.Checked

                    "migrated" ->
                        Decode.succeed Bullet.Migrated

                    _ ->
                        Decode.fail ("expected unchecked, checked or migrated but got " ++ taskStateString)
            )
