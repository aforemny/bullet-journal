module Type.Bullet exposing (..)

import Date exposing (Date)
import Html.Attributes as Html
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Material.List as Lists
import Material.Options as Options exposing (styled, cs, css, when)
import Parse
import Parse.Decode
import Parse.Encode
import Private.ObjectId as ObjectId
import Private.Pointer as Pointer
import Task
import Task exposing (Task)


type alias Bullet =
    { spreadClass : String
    , spreadId : Parse.ObjectId Any
    , state : State
    , text : String
    }


empty : String -> Parse.ObjectId Any -> Bullet
empty =
    emptyNote


emptyNote : String -> Parse.ObjectId Any -> Bullet
emptyNote spreadClass spreadId =
    { spreadClass = spreadClass
    , spreadId = spreadId
    , state = Note
    , text = ""
    }


emptyTask : String -> Parse.ObjectId Any -> Bullet
emptyTask spreadClass spreadId =
    { spreadClass = spreadClass
    , spreadId = spreadId
    , state = Task Unchecked
    , text = ""
    }


emptyEvent : String -> Parse.ObjectId Any -> Bullet
emptyEvent spreadClass spreadId =
    { spreadClass = spreadClass
    , spreadId = spreadId
    , state = Event
    , text = ""
    }


fromParseObject : Parse.Object Bullet -> Bullet
fromParseObject bullet =
    { spreadClass = bullet.spreadClass
    , spreadId = bullet.spreadId
    , state = bullet.state
    , text = bullet.text
    }


encode : Bullet -> Value
encode bullet =
    Encode.object
        [ ( "spreadClass", Encode.string bullet.spreadClass )
        , ( "spreadId", Parse.Encode.objectId bullet.spreadId )
        , ( "state", encodeState bullet.state )
        , ( "text", Encode.string bullet.text )
        ]


decode : Decoder (Parse.Object Bullet)
decode =
    Decode.succeed
        (\objectId createdAt updatedAt spreadClass spreadId state text ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , spreadClass = spreadClass
            , spreadId = spreadId
            , state = state
            , text = text
            }
        )
        |> Decode.required "objectId" Parse.Decode.objectId
        |> Decode.required "createdAt" Parse.Decode.date
        |> Decode.required "updatedAt" Parse.Decode.date
        |> Decode.required "spreadClass" Decode.string
        |> Decode.required "spreadId" Parse.Decode.objectId
        |> Decode.required "state" decodeState
        |> Decode.required "text" Decode.string


type Any
    = Any


anyObjectId : Parse.ObjectId a -> Parse.ObjectId Any
anyObjectId =
    ObjectId.toString >> ObjectId.fromString


castObjectId : Parse.ObjectId Any -> Parse.ObjectId b
castObjectId =
    ObjectId.toString >> ObjectId.fromString


type State
    = Event
    | Note
    | Task TaskState


encodeState : State -> Value
encodeState state =
    case state of
        Event ->
            Encode.object [ ( "ctor", Encode.string "event" ) ]

        Note ->
            Encode.object [ ( "ctor", Encode.string "note" ) ]

        Task taskState ->
            Encode.object
                [ ( "ctor", Encode.string "task" )
                , ( "taskState", encodeTaskState taskState )
                ]


decodeState : Decoder State
decodeState =
    let
        decodeEvent =
            Decode.succeed Event

        decodeNote =
            Decode.succeed Note

        decodeTask =
            Decode.map Task (Decode.at [ "taskState" ] decodeTaskState)
    in
        Decode.at [ "ctor" ] Decode.string
            |> Decode.andThen
                (\ctor ->
                    case ctor of
                        "event" ->
                            decodeEvent

                        "note" ->
                            decodeNote

                        "task" ->
                            decodeTask

                        _ ->
                            Decode.fail ("expected event, note or task but got " ++ ctor)
                )


type TaskState
    = Unchecked
    | Checked
    | Migrated


encodeTaskState : TaskState -> Value
encodeTaskState taskState =
    Encode.string <|
        case taskState of
            Unchecked ->
                "unchecked"

            Checked ->
                "checked"

            Migrated ->
                "migrated"


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
                |> \query ->
                    { query
                        | whereClause =
                            Parse.and
                                [ Parse.equalTo "spreadClass" (Encode.string spreadClass)
                                , Parse.equalTo "spreadId" (Parse.Encode.objectId spreadId)
                                ]
                    }
            )
        )


create :
    Parse.Config
    -> Bullet
    -> Task Parse.Error { objectId : Parse.ObjectId Bullet, createdAt : Date }
create parse bullet =
    Parse.toTask parse (Parse.create "Bullet" encode bullet)


update :
    Parse.Config
    -> Parse.ObjectId Bullet
    -> Bullet
    -> Task Parse.Error { updatedAt : Date }
update parse bulletId bullet =
    Parse.toTask parse (Parse.update "Bullet" encode bulletId bullet)


type alias Config msg =
    { additionalOptions : List (Lists.Property msg)
    }


view : Config msg -> Bullet -> Html msg
view config bullet =
    Lists.li
        (cs "bullet"
            :: (case bullet.state of
                    Event ->
                        cs "bullet--event"

                    Note ->
                        cs "bullet--note"

                    Task Unchecked ->
                        cs "bullet--task bullet--task--unchecked"

                    Task Checked ->
                        cs "bullet--task bullet--task--checked"

                    Task Migrated ->
                        cs "bullet--task bullet--task--migrated"
               )
            :: config.additionalOptions
        )
        [ Lists.graphicIcon
            []
            (case bullet.state of
                Event ->
                    "radio_button_unchecked"

                Note ->
                    "indeterminate_check_box"

                Task Unchecked ->
                    "check_box_outline_blank"

                Task Checked ->
                    "check_box"

                Task Migrated ->
                    "check_box"
            )
        , Lists.text []
            [ text bullet.text
            ]
        ]
