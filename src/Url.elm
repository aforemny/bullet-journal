module Url exposing (..)

import Navigation
import Parse
import Private.ObjectId as ObjectId
import String
import Type.Bullet as Bullet
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)
import Type.DailySpread as DailySpread exposing (DailySpread)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)
import UrlParser exposing (..)


type Url
    = Index
    | CollectionSpread (Parse.ObjectId CollectionSpread)
    | DailySpread (Parse.ObjectId DailySpread)
    | MonthlySpread (Parse.ObjectId MonthlySpread)
    | NewBullet String String (Parse.ObjectId Bullet.Any)
    | NotFound String


toString : Url -> String
toString url =
    String.cons '#' <|
        case url of
            Index ->
                ""

            MonthlySpread objectId ->
                "monthly-spread/" ++ ObjectId.toString objectId

            DailySpread objectId ->
                "daily-spread/" ++ ObjectId.toString objectId

            CollectionSpread objectId ->
                "collection-spread/" ++ ObjectId.toString objectId

            NewBullet route className objectId ->
                route ++ "/" ++ ObjectId.toString objectId ++ "/bullet/new"

            NotFound hash ->
                hash


fromLocation : Navigation.Location -> Url
fromLocation location =
    case
        parseHash parseUrl location
            |> Maybe.withDefault (NotFound (String.dropLeft 1 location.hash))
    of
        NotFound "" ->
            Index

        url ->
            url


parseUrl =
    let
        objectId =
            map ObjectId.fromString string
    in
        oneOf
            [ map Index (s "")
            , map (NewBullet "collection-spread" "CollectionSpread")
                (s "collection-spread" </> objectId </> s "bullet" </> s "new")
            , map (NewBullet "monthly-spread" "MonthlySpread")
                (s "monthly-spread" </> objectId </> s "bullet" </> s "new")
            , map (NewBullet "daily-spread" "DailySpread")
                (s "daily-spread" </> objectId </> s "bullet" </> s "new")
            , map CollectionSpread (s "collection-spread" </> objectId)
            , map DailySpread (s "daily-spread" </> objectId)
            , map MonthlySpread (s "monthly-spread" </> objectId)
            ]
