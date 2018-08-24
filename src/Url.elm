module Url exposing (..)

import Navigation
import Parse
import Private.ObjectId as ObjectId
import String
import Type.Bullet as Bullet exposing (Bullet)
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)
import Type.DailySpread as DailySpread exposing (DailySpread)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)
import UrlParser exposing (..)


type Url
    = Index
    | Start
    | CollectionSpread (Parse.ObjectId CollectionSpread)
    | EditCollectionSpread (Parse.ObjectId CollectionSpread)
    | DailySpread (Parse.ObjectId DailySpread)
    | EditDailySpread (Parse.ObjectId DailySpread)
    | MonthlySpread (Parse.ObjectId MonthlySpread)
    | EditMonthlySpread (Parse.ObjectId MonthlySpread)
    | EditBullet (Maybe (Parse.ObjectId Bullet))
    | NotFound String


toString : Url -> String
toString url =
    String.cons '#' <|
        case url of
            Start ->
                ""

            Index ->
                "index"

            MonthlySpread objectId ->
                "monthly-spread/" ++ ObjectId.toString objectId

            EditMonthlySpread objectId ->
                "monthly-spread/" ++ ObjectId.toString objectId ++ "/edit"

            DailySpread objectId ->
                "daily-spread/" ++ ObjectId.toString objectId

            EditDailySpread objectId ->
                "daily-spread/" ++ ObjectId.toString objectId ++ "/edit"

            CollectionSpread objectId ->
                "collection-spread/" ++ ObjectId.toString objectId

            EditCollectionSpread objectId ->
                "collection-spread/" ++ ObjectId.toString objectId ++ "/edit"

            EditBullet Nothing ->
                "bullet/new"

            EditBullet (Just bulletId) ->
                "bullet/" ++ ObjectId.toString bulletId

            NotFound hash ->
                hash


fromLocation : Navigation.Location -> Url
fromLocation location =
    case
        parseHash parseUrl location
            |> Maybe.withDefault (NotFound (String.dropLeft 1 location.hash))
    of
        NotFound "" ->
            Start

        url ->
            url


parseUrl =
    let
        objectId =
            map ObjectId.fromString string
    in
    oneOf
        [ map Start (s "")
        , map Index (s "index")
        , map (EditBullet Nothing) (s "bullet" </> s "new")
        , map (EditBullet << Just) (s "bullet" </> objectId)
        , map EditCollectionSpread (s "collection-spread" </> objectId </> s "edit")
        , map EditDailySpread (s "daily-spread" </> objectId </> s "edit")
        , map EditMonthlySpread (s "monthly-spread" </> objectId </> s "edit")
        , map CollectionSpread (s "collection-spread" </> objectId)
        , map DailySpread (s "daily-spread" </> objectId)
        , map MonthlySpread (s "monthly-spread" </> objectId)
        ]
