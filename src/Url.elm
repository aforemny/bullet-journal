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
    | CollectionSpread (Parse.ObjectId CollectionSpread)
    | DailySpread (Parse.ObjectId DailySpread)
    | MonthlySpread (Parse.ObjectId MonthlySpread)
    | EditBullet String String (Parse.ObjectId Bullet.Any) (Maybe (Parse.ObjectId Bullet))
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

            EditBullet route className spreadId Nothing ->
                route ++ "/" ++ ObjectId.toString spreadId ++ "/bullet/new"

            EditBullet route className spreadId (Just bulletId) ->
                route
                    ++ "/"
                    ++ ObjectId.toString spreadId
                    ++ "/bullet/"
                    ++ ObjectId.toString bulletId

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
            , map
                (\spreadId ->
                    EditBullet "collection-spread" "CollectionSpread" spreadId Nothing
                )
                (s "collection-spread" </> objectId </> s "bullet" </> s "new")
            , map
                (\spreadId ->
                    EditBullet "monthly-spread" "MonthlySpread" spreadId Nothing
                )
                (s "monthly-spread" </> objectId </> s "bullet" </> s "new")
            , map
                (\spreadId ->
                    EditBullet "daily-spread" "DailySpread" spreadId Nothing
                )
                (s "daily-spread" </> objectId </> s "bullet" </> s "new")
            , map
                (\spreadId bulletId ->
                    EditBullet "collection-spread" "CollectionSpread" spreadId (Just bulletId)
                )
                (s "collection-spread" </> objectId </> s "bullet" </> objectId)
            , map
                (\spreadId bulletId ->
                    EditBullet "monthly-spread" "MonthlySpread" spreadId (Just bulletId)
                )
                (s "monthly-spread" </> objectId </> s "bullet" </> objectId)
            , map
                (\spreadId bulletId ->
                    EditBullet "daily-spread" "DailySpread" spreadId (Just bulletId)
                )
                (s "daily-spread" </> objectId </> s "bullet" </> objectId)
            , map CollectionSpread (s "collection-spread" </> objectId)
            , map DailySpread (s "daily-spread" </> objectId)
            , map MonthlySpread (s "monthly-spread" </> objectId)
            ]
