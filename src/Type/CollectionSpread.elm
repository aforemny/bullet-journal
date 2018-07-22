module Type.CollectionSpread exposing (..)

import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Parse
import Parse.Decode
import Parse.Encode
import Task exposing (Task)
import Type.Bullet as Bullet exposing (Bullet)


type alias CollectionSpread =
    { title : String
    , createdAt : Date
    }


empty : String -> CollectionSpread
empty title =
    { title = title
    , createdAt = Date.fromTime 0
    }


encode : CollectionSpread -> Value
encode collectionSpread =
    Encode.object
        [ ( "title", Encode.string collectionSpread.title )
        ]


decode : Decoder (Parse.Object CollectionSpread)
decode =
    Decode.decode
        (\objectId createdAt updatedAt title ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , title = title
            }
        )
        |> Decode.required "objectId" Parse.Decode.objectId
        |> Decode.required "createdAt" Parse.Decode.date
        |> Decode.required "updatedAt" Parse.Decode.date
        |> Decode.required "title" Decode.string


fromParseObject : Parse.Object CollectionSpread -> CollectionSpread
fromParseObject collectionSpread =
    { title = collectionSpread.title
    , createdAt = collectionSpread.createdAt
    }


canonicalDate : CollectionSpread -> ( Int, Int, Int )
canonicalDate collectionSpread =
    ( Date.year collectionSpread.createdAt
    , case Date.month collectionSpread.createdAt of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12
    , Date.day collectionSpread.createdAt
    )


title : CollectionSpread -> String
title collectionSpread =
    collectionSpread.title


create :
    Parse.Config
    -> CollectionSpread
    -> Task Parse.Error (Parse.ObjectId CollectionSpread)
create parse collectionSpread =
    Task.map .objectId <|
        Parse.toTask parse
            (Parse.create "CollectionSpread" encode collectionSpread)


get :
    Parse.Config
    -> Parse.ObjectId CollectionSpread
    -> Task Parse.Error (Parse.Object CollectionSpread)
get parse objectId =
    Parse.toTask parse (Parse.get "CollectionSpread" decode objectId)
