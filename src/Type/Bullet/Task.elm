module Type.Bullet.Task exposing (..)

import Html.Attributes as Html
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Parse
import Parse.Decode
import Parse.Encode
import Private.Pointer as Pointer
import Task


type alias Task =
    { spread : String
    , text : String
    , state : State
    }


type State
    = Unchecked
    | Checked
    | Migrated


encode : Task -> Value
encode task =
    Encode.object
        [ ( "spread", Encode.string task.spread )
        , ( "text", Encode.string task.text )
        , ( "state", encodeState task.state )
        ]


decode : Decoder (Parse.Object Task)
decode =
    Decode.succeed
        (\objectId createdAt updatedAt spread text state ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , spread = spread
            , text = text
            , state = state
            }
        )
        |> Decode.required "objectId" Parse.Decode.objectId
        |> Decode.required "createdAt" Parse.Decode.date
        |> Decode.required "updatedAt" Parse.Decode.date
        |> Decode.required "spread" Decode.string
        |> Decode.required "text" Decode.string
        |> Decode.required "state" decodeState


encodeState : State -> Value
encodeState state =
    Encode.string <|
        case state of
            Unchecked ->
                "unchecked"

            Checked ->
                "checked"

            Migrated ->
                "migrated"


decodeState : Decoder State
decodeState =
    Decode.string
        |> Decode.andThen
            (\stateString ->
                case stateString of
                    "unchecked" ->
                        Decode.succeed Unchecked

                    "checked" ->
                        Decode.succeed Checked

                    "migrated" ->
                        Decode.succeed Migrated

                    _ ->
                        Decode.fail ("expected unchecked, checked or migrated, but got " ++ stateString)
            )
