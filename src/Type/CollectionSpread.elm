module Type.CollectionSpread exposing (CollectionSpread, canonicalDate, create, decode, delete, empty, encode, fromParseObject, get, title, update)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Parse
import Parse.Decode
import Parse.Encode
import Task exposing (Task)
import Time
import Type.Bullet as Bullet exposing (Bullet)


type alias CollectionSpread =
    { title : String
    , createdAt : Time.Posix
    }


empty : String -> CollectionSpread
empty title_ =
    { title = title_
    , createdAt = Time.millisToPosix 0
    }


encode : CollectionSpread -> Value
encode collectionSpread =
    Encode.object
        [ ( "title", Encode.string collectionSpread.title )
        ]


decode : Decoder (Parse.Object CollectionSpread)
decode =
    Decode.succeed
        (\objectId createdAt updatedAt title_ ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , title = title_
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


canonicalDate : Time.Zone -> CollectionSpread -> ( Int, Int, Int )
canonicalDate tz collectionSpread =
    ( Time.toYear tz collectionSpread.createdAt
    , case Time.toMonth tz collectionSpread.createdAt of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
    , Time.toDay tz collectionSpread.createdAt
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


update :
    Parse.Config
    -> Parse.ObjectId CollectionSpread
    -> CollectionSpread
    -> Task Parse.Error { updatedAt : Time.Posix }
update parse collectionSpreadId collectionSpread =
    Parse.toTask parse
        (Parse.update "CollectionSpread" encode collectionSpreadId collectionSpread)


delete : Parse.Config -> Parse.ObjectId CollectionSpread -> Task Parse.Error {}
delete parse collectionSpreadId =
    Parse.toTask parse
        (Parse.delete "CollectionSpread" collectionSpreadId)
