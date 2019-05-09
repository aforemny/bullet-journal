module Route exposing (Route(..), fromUrl, toString)

import Parse
import Parse.Private.ObjectId as ObjectId
import String
import Type.Bullet as Bullet exposing (Bullet)
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)
import Type.DailySpread as DailySpread exposing (DailySpread)
import Type.MonthlySpread as MonthlySpread exposing (MonthlySpread)
import Url exposing (Url)
import Url.Parser exposing ((</>), s)


type Route
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


toString : Route -> String
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


fromUrl : Url -> Route
fromUrl url =
    case
        Url.Parser.parse parseUrl
            { url | path = Maybe.withDefault "" url.fragment }
            |> Maybe.withDefault
                (NotFound (Maybe.withDefault "" url.fragment))
    of
        NotFound "" ->
            Start

        otherUrl ->
            otherUrl


parseUrl =
    let
        objectId =
            Url.Parser.map ObjectId.fromString Url.Parser.string
    in
    Url.Parser.oneOf
        [ Url.Parser.map Start (s "")
        , Url.Parser.map Index (s "index")
        , Url.Parser.map (EditBullet Nothing) (s "bullet" </> s "new")
        , Url.Parser.map (EditBullet << Just) (s "bullet" </> objectId)
        , Url.Parser.map EditCollectionSpread
            (s "collection-spread" </> objectId </> s "edit")
        , Url.Parser.map EditDailySpread (s "daily-spread" </> objectId </> s "edit")
        , Url.Parser.map EditMonthlySpread (s "monthly-spread" </> objectId </> s "edit")
        , Url.Parser.map CollectionSpread (s "collection-spread" </> objectId)
        , Url.Parser.map DailySpread (s "daily-spread" </> objectId)
        , Url.Parser.map MonthlySpread (s "monthly-spread" </> objectId)
        ]
