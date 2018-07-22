module Type.Bullet.Note exposing (..)

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


type alias Note =
    { spread : String
    , text : String
    }


encode : Note -> Value
encode note =
    Encode.object
        [ ( "spread", Encode.string note.spread )
        , ( "text", Encode.string note.text )
        ]


decode : Decoder (Parse.Object Note)
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
