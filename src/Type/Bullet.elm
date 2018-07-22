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
    { spread : Parse.Pointer Any
    , state : State
    , text : String
    }


empty : Parse.Pointer Any -> Bullet
empty =
    emptyNote


emptyNote : Parse.Pointer Any -> Bullet
emptyNote spread =
    { spread = spread
    , state = Note
    , text = ""
    }


emptyTask : Parse.Pointer Any -> Bullet
emptyTask spread =
    { spread = spread
    , state = Task Unchecked
    , text = ""
    }


emptyEvent : Parse.Pointer Any -> Bullet
emptyEvent spread =
    { spread = spread
    , state = Event
    , text = ""
    }


fromParseObject : Parse.Object Bullet -> Bullet
fromParseObject bullet =
    { spread = bullet.spread
    , state = bullet.state
    , text = bullet.text
    }


encode : Bullet -> Value
encode bullet =
    Encode.object
        [ ( "spread", encodeSpread bullet.spread )
        , ( "state", encodeState bullet.state )
        , ( "text", Encode.string bullet.text )
        ]


encodeSpread : Parse.Pointer spread -> Value
encodeSpread spread =
    Encode.object
        [ ( "className", Encode.string (Pointer.className spread) )
        , ( "objectId", Encode.string (ObjectId.toString (Pointer.objectId spread)) )
        ]


decode : Decoder (Parse.Object Bullet)
decode =
    Decode.succeed
        (\objectId createdAt updatedAt spread state text ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , spread = spread
            , state = state
            , text = text
            }
        )
        |> Decode.required "objectId" Parse.Decode.objectId
        |> Decode.required "createdAt" Parse.Decode.date
        |> Decode.required "updatedAt" Parse.Decode.date
        |> Decode.required "spread" decodeSpread
        |> Decode.required "state" decodeState
        |> Decode.required "text" Decode.string


decodeSpread : Decoder (Parse.Pointer Any)
decodeSpread =
    Decode.map2 Parse.pointer
        (Decode.at [ "className" ] Decode.string)
        (Decode.map ObjectId.fromString (Decode.at [ "objectId" ] Decode.string))


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
    -> Parse.Pointer spread
    -> Task Parse.Error (List (Parse.Object Bullet))
get parse spread =
    Parse.toTask parse
        (Parse.query decode
            (Parse.emptyQuery "Bullet"
                |> \query ->
                    { query | whereClause = Parse.equalTo "spread" (encodeSpread spread) }
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
