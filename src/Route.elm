module Route exposing (Route(..), fromUrl, toString)

import Parse
import Parse.Private.ObjectId as ObjectId
import String
import Type.Bullet as Bullet exposing (Bullet)
import Type.CollectionSpread exposing (CollectionSpread)
import Url exposing (Url)
import Url.Parser exposing ((</>), int, s)


type Route
    = Start
    | DailySpread { year : Int, month : Int, dayOfMonth : Int }
    | MonthlySpread { year : Int, month : Int }
    | CollectionSpread (Parse.ObjectId CollectionSpread)
    | EditBullet (Maybe (Parse.ObjectId Bullet))
    | EditCollectionSpread (Maybe (Parse.ObjectId CollectionSpread))
    | TableOfContent
    | NotFound String


toString : Route -> String
toString url =
    String.cons '#' <|
        case url of
            Start ->
                ""

            MonthlySpread { year, month } ->
                "monthly-spread/"
                    ++ String.fromInt year
                    ++ "/"
                    ++ String.fromInt month

            DailySpread { year, month, dayOfMonth } ->
                "daily-spread/"
                    ++ String.fromInt year
                    ++ "/"
                    ++ String.fromInt month
                    ++ "/"
                    ++ String.fromInt dayOfMonth

            CollectionSpread collectionSpreadId ->
                "collection-spread/" ++ ObjectId.toString collectionSpreadId

            EditCollectionSpread Nothing ->
                "collection-spread/new"

            EditCollectionSpread (Just collectionSpreadId) ->
                "collection-spread/edit/" ++ ObjectId.toString collectionSpreadId

            EditBullet Nothing ->
                "bullet/new"

            EditBullet (Just bulletId) ->
                "bullet/" ++ ObjectId.toString bulletId

            TableOfContent ->
                "table-of-content"

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
        , Url.Parser.map (EditCollectionSpread Nothing) (s "collection-spread" </> s "new")
        , Url.Parser.map (EditCollectionSpread << Just)
            (s "collection-spread" </> s "edit" </> objectId)
        , Url.Parser.map CollectionSpread
            (s "collection-spread" </> objectId)
        , Url.Parser.map (EditBullet Nothing) (s "bullet" </> s "new")
        , Url.Parser.map (EditBullet << Just) (s "bullet" </> objectId)
        , Url.Parser.map
            (\year month dayOfMonth ->
                DailySpread { year = year, month = month, dayOfMonth = dayOfMonth }
            )
            (s "daily-spread" </> int </> int </> int)
        , Url.Parser.map
            (\year month ->
                MonthlySpread { year = year, month = month }
            )
            (s "monthly-spread" </> int </> int)
        , Url.Parser.map TableOfContent (s "table-of-content")
        ]
