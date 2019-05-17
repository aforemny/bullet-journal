module Type.Bullet exposing (Bullet, Config, Ctor(..), Date(..), TaskState(..), create, decode, delete, emptyEvent, emptyNote, emptyTask, encode, fromParseObject, get, getOf, update, view)

import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Material.Icon exposing (icon, iconConfig)
import Material.List exposing (listItem, listItemConfig, listItemGraphic, listItemText)
import Parse
import Parse.Decode
import Parse.Encode
import Parse.Private.ObjectId as ObjectId
import Parse.Private.Pointer as Pointer
import Task exposing (Task)
import Time


type alias Bullet =
    { ctor : Ctor
    , date : Date
    , text : String
    , taskState : Maybe TaskState
    }


type Ctor
    = Task
    | Event
    | Note


type Date
    = MonthDate { year : Int, month : Int }
    | DayDate { year : Int, month : Int, dayOfMonth : Int }


type TaskState
    = Unchecked
    | Checked
    | Migrated


emptyNote : Bullet
emptyNote =
    { ctor = Note
    , date = DayDate { year = 1970, month = 1, dayOfMonth = 1 }
    , text = ""
    , taskState = Nothing
    }


emptyTask : Bullet
emptyTask =
    { ctor = Task
    , date = DayDate { year = 1970, month = 1, dayOfMonth = 1 }
    , text = ""
    , taskState = Nothing
    }


emptyEvent : Bullet
emptyEvent =
    { ctor = Event
    , date = DayDate { year = 1970, month = 1, dayOfMonth = 1 }
    , text = ""
    , taskState = Nothing
    }


fromParseObject : Parse.Object Bullet -> Bullet
fromParseObject bullet =
    { ctor = bullet.ctor
    , text = bullet.text
    , date = bullet.date
    , taskState = bullet.taskState
    }


encode : Bullet -> Value
encode bullet =
    Encode.object
        [ ( "ctor", encodeCtor bullet.ctor )
        , ( "text", Encode.string bullet.text )
        , ( "date", encodeDate bullet.date )
        , ( "taskState", encodeTaskState bullet.taskState )
        ]


encodeCtor ctor =
    Encode.string <|
        case ctor of
            Event ->
                "event"

            Note ->
                "note"

            Task ->
                "task"


encodeDate date =
    case date of
        MonthDate { year, month } ->
            Encode.object
                [ ( "year", Encode.int year )
                , ( "month", Encode.int month )
                ]

        DayDate { year, month, dayOfMonth } ->
            Encode.object
                [ ( "year", Encode.int year )
                , ( "month", Encode.int month )
                , ( "dayOfMonth", Encode.int dayOfMonth )
                ]


decodeDate =
    Decode.succeed
        (\year month maybeDayOfMonth ->
            case maybeDayOfMonth of
                Just dayOfMonth ->
                    DayDate { year = year, month = month, dayOfMonth = dayOfMonth }

                Nothing ->
                    MonthDate { year = year, month = month }
        )
        |> Decode.required "year" Decode.int
        |> Decode.required "month" Decode.int
        |> Decode.optional "dayOfMonth" (Decode.map Just Decode.int) Nothing


decodeCtor =
    Decode.string
        |> Decode.andThen
            (\ctor ->
                case ctor of
                    "task" ->
                        Decode.succeed Task

                    "event" ->
                        Decode.succeed Event

                    "note" ->
                        Decode.succeed Note

                    _ ->
                        Decode.fail ("unknown ctor: " ++ ctor)
            )


decode : Decoder (Parse.Object Bullet)
decode =
    Decode.succeed
        (\objectId createdAt updatedAt ctor date text taskState ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , ctor = ctor
            , date = date
            , text = text
            , taskState = taskState
            }
        )
        |> Decode.required "objectId" Parse.Decode.objectId
        |> Decode.required "createdAt" Parse.Decode.date
        |> Decode.required "updatedAt" Parse.Decode.date
        |> Decode.required "ctor" decodeCtor
        |> Decode.required "date" decodeDate
        |> Decode.required "text" Decode.string
        |> Decode.optional "taskState" (Decode.map Just decodeTaskState) Nothing


encodeTaskState : Maybe TaskState -> Value
encodeTaskState maybeTaskState =
    maybeTaskState
        |> Maybe.map
            (\taskState ->
                Encode.string <|
                    case taskState of
                        Unchecked ->
                            "unchecked"

                        Checked ->
                            "checked"

                        Migrated ->
                            "migrated"
            )
        |> Maybe.withDefault Encode.null


decodeTaskState : Decoder TaskState
decodeTaskState =
    Decode.string
        |> Decode.andThen
            (\taskStateString ->
                case taskStateString of
                    "unchecked" ->
                        Decode.succeed Unchecked

                    "checked" ->
                        Decode.succeed Checked

                    "migrated" ->
                        Decode.succeed Migrated

                    _ ->
                        Decode.fail ("expected unchecked, checked or migrated, but got " ++ taskStateString)
            )


get :
    Parse.Config
    -> Parse.ObjectId Bullet
    -> Task Parse.Error (Parse.Object Bullet)
get parse objectId =
    Parse.toTask parse (Parse.get "Bullet" decode objectId)


getOf :
    Parse.Config
    -> String
    -> Parse.ObjectId spreadId
    -> Task Parse.Error (List (Parse.Object Bullet))
getOf parse spreadClass spreadId =
    Parse.toTask parse
        (Parse.query decode
            (Parse.emptyQuery "Bullet"
                |> (\query ->
                        { query
                            | whereClause =
                                Parse.and
                                    [ Parse.equalTo "spreadClass" (Encode.string spreadClass)
                                    , Parse.equalTo "spreadId" (Parse.Encode.objectId spreadId)
                                    ]
                        }
                   )
            )
        )


create :
    Parse.Config
    -> Bullet
    -> Task Parse.Error { objectId : Parse.ObjectId Bullet, createdAt : Time.Posix }
create parse bullet =
    Parse.toTask parse (Parse.create "Bullet" encode bullet)


update :
    Parse.Config
    -> Parse.ObjectId Bullet
    -> Bullet
    -> Task Parse.Error { updatedAt : Time.Posix }
update parse bulletId bullet =
    Parse.toTask parse (Parse.update "Bullet" encode bulletId bullet)


delete :
    Parse.Config
    -> Parse.ObjectId Bullet
    -> Task Parse.Error {}
delete parse bulletId =
    Parse.toTask parse (Parse.delete "Bullet" bulletId)


type alias Config msg =
    { additionalOptions : List (Html.Attribute msg)
    }


view : Config msg -> Bullet -> Html msg
view config bullet =
    let
        stateCs =
            case ( bullet.ctor, bullet.taskState ) of
                ( Event, _ ) ->
                    class "bullet--event"

                ( Note, _ ) ->
                    class "bullet--note"

                ( Task, Just Checked ) ->
                    class "bullet--task bullet--task--checked"

                ( Task, Just Migrated ) ->
                    class "bullet--task bullet--task--migrated"

                ( Task, _ ) ->
                    class "bullet--task bullet--task--unchecked"
    in
    listItem
        { listItemConfig
            | additionalAttributes =
                [ class "bullet", stateCs ] ++ config.additionalOptions
        }
        [ listItemGraphic []
            [ case ( bullet.ctor, bullet.taskState ) of
                ( Event, _ ) ->
                    icon iconConfig "radio_button_unchecked"

                ( Note, _ ) ->
                    icon iconConfig "indeterminate_check_box"

                ( Task, Just Checked ) ->
                    icon iconConfig "check_box"

                ( Task, Just Migrated ) ->
                    icon iconConfig "check_box"

                ( Task, _ ) ->
                    icon iconConfig "check_box_outline_blank"
            ]
        , listItemText [] [ text bullet.text ]
        ]
