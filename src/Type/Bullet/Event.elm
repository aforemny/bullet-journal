module Type.Bullet.Event exposing (..)

import Html.Attributes as Html
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Parse
import Parse.Decode
import Parse.Encode
import Private.Pointer as Pointer
import Task exposing (Task)


type alias Event =
    { spread : String
    , text : String
    }


encode : Event -> Value
encode event =
    Encode.object
        [ ( "spread", Encode.string event.spread )
        , ( "text", Encode.string event.text )
        ]


decode : Decoder (Parse.Object Event)
decode =
    Decode.succeed
        (\objectId createdAt updatedAt spread text ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , spread = spread
            , text = text
            }
        )
        |> Decode.required "objectId" Parse.Decode.objectId
        |> Decode.required "createdAt" Parse.Decode.date
        |> Decode.required "updatedAt" Parse.Decode.date
        |> Decode.required "spread" Decode.string
        |> Decode.required "text" Decode.string
