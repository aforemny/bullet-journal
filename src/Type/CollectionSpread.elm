module Type.CollectionSpread exposing (..)

import Date exposing (Date)
import Type.Bullet as Bullet exposing (Bullet)


type alias CollectionSpread =
    { id : Id
    , createdDate : Date
    , title : Title
    , bullets : List Bullet
    }


type alias Id =
    String


type alias Title =
    String


empty : Id -> Date -> CollectionSpread
empty id date =
    { id = id
    , createdDate = date
    , title = ""
    , bullets = []
    }


canonicalDate : CollectionSpread -> ( Int, Int, Int )
canonicalDate collectionSpread =
    ( Date.year collectionSpread.createdDate
    , case Date.month collectionSpread.createdDate of
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
    , Date.day collectionSpread.createdDate
    )


title : CollectionSpread -> String
title collectionSpread =
    collectionSpread.title
