module Type.Bullet exposing (..)

import Date exposing (Date)
import Html.Attributes as Html
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Parse
import Parse.Decode
import Parse.Encode
import Private.ObjectId as ObjectId
import Private.Pointer as Pointer
import Task
import Task exposing (Task)


type alias Bullet =
    { spreadClass : String
    , spread : Parse.ObjectId Any
    , state : State
    , text : String
    }


empty : String -> Parse.ObjectId Any -> Bullet
empty =
    emptyNote


emptyNote : String -> Parse.ObjectId Any -> Bullet
emptyNote spreadClass spread =
    { spreadClass = spreadClass
    , spread = spread
    , state = Note
    , text = ""
    }


emptyTask : String -> Parse.ObjectId Any -> Bullet
emptyTask spreadClass spread =
    { spreadClass = spreadClass
    , spread = spread
    , state = Task Unchecked
    , text = ""
    }


emptyEvent : String -> Parse.ObjectId Any -> Bullet
emptyEvent spreadClass spread =
    { spreadClass = spreadClass
    , spread = spread
    , state = Event
    , text = ""
    }


fromParseObject : Parse.Object Bullet -> Bullet
fromParseObject bullet =
    { spreadClass = bullet.spreadClass
    , spread = bullet.spread
    , state = bullet.state
    , text = bullet.text
    }


encode : Bullet -> Value
encode bullet =
    Encode.object
        [ ( "spreadClass", Encode.string bullet.spreadClass )
        , ( "spread", Parse.Encode.objectId bullet.spread )
        , ( "state", encodeState bullet.state )
        , ( "text", Encode.string bullet.text )
        ]


decode : Decoder (Parse.Object Bullet)
decode =
    Decode.succeed
        (\objectId createdAt updatedAt spreadClass spread state text ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , spreadClass = spreadClass
            , spread = spread
            , state = state
            , text = text
            }
        )
        |> Decode.required "objectId" Parse.Decode.objectId
        |> Decode.required "createdAt" Parse.Decode.date
        |> Decode.required "updatedAt" Parse.Decode.date
        |> Decode.required "spreadClass" Decode.string
        |> Decode.required "spread" Parse.Decode.objectId
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
    -> String
    -> Parse.ObjectId spread
    -> Task Parse.Error (List (Parse.Object Bullet))
get parse spreadClass spread =
    Parse.toTask parse
        (Parse.query decode
            (Parse.emptyQuery "Bullet"
                |> \query ->
                    { query
                        | whereClause =
                            Parse.and
                            [ Parse.equalTo "spreadClass" (Encode.string spreadClass)
                            , Parse.equalTo "spread" (Parse.Encode.objectId spread)
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


type alias Config msg =
    { node : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , additionalAttributes : List (Html.Attribute msg)
    }


view : Config msg -> Bullet -> Html msg
view config bullet =
    config.node
        (Html.class "bullet"
            :: (case bullet.state of
                    Event ->
                        Html.class "bullet--event"

                    Note ->
                        Html.class "bullet--note"

                    Task Unchecked ->
                        Html.class "bullet--task bullet--task--unchecked"

                    Task Checked ->
                        Html.class "bullet--task bullet--task--checked"

                    Task Migrated ->
                        Html.class "bullet--task bullet--task--migrated"
               )
            :: config.additionalAttributes
        )
        [ text bullet.text
        ]
