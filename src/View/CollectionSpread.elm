module View.CollectionSpread exposing (..)

import Date exposing (Date)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)
import Material
import Material.Button as Button
import Material.Toolbar as Toolbar
import Navigation
import Parse
import Private.ObjectId as ObjectId
import Task exposing (Task)
import Type.Bullet as Bullet exposing (Bullet)
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)
import Url
import View


type alias Model msg =
    { mdc : Material.Model msg
    , collectionSpread : Maybe (Parse.Object CollectionSpread)
    , bullets : List (Parse.Object Bullet)
    , error : Maybe Parse.Error
    }


defaultModel : Model msg
defaultModel =
    { mdc = Material.defaultModel
    , collectionSpread = Nothing
    , bullets = []
    , error = Nothing
    }


type Msg msg
    = Mdc (Material.Msg msg)
    | CollectionSpreadResult (Result Parse.Error (Parse.Object CollectionSpread))
    | BulletsResult (Result Parse.Error (List (Parse.Object Bullet)))
    | NewBulletClicked



--    | TitleChanged String
--    | BulletChanged Index String


init :
    (Msg msg -> msg)
    -> View.Config msg
    -> Parse.ObjectId CollectionSpread
    -> Model msg
    -> ( Model msg, Cmd msg )
init lift viewConfig objectId model =
    ( defaultModel
    , Cmd.batch
        [ Material.init (lift << Mdc)
        , Task.attempt (lift << CollectionSpreadResult)
            (CollectionSpread.get viewConfig.parse objectId)
        , Task.attempt (lift << BulletsResult)
            (Bullet.get viewConfig.parse "CollectionSpread" objectId)
        ]
    )


subscriptions : (Msg msg -> msg) -> Model msg -> Sub msg
subscriptions lift model =
    Material.subscriptions (lift << Mdc) model


update : (Msg msg -> msg) -> Msg msg -> Model msg -> ( Model msg, Cmd msg )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (lift << Mdc) msg_ model

        BulletsResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        BulletsResult (Ok bullets) ->
            ( { model | bullets = bullets }, Cmd.none )

        CollectionSpreadResult (Err err) ->
            ( { model | error = Just err }, Cmd.none )

        CollectionSpreadResult (Ok collectionSpread) ->
            ( { model | collectionSpread = Just collectionSpread }, Cmd.none )

        NewBulletClicked ->
            ( model
            , model.collectionSpread
                |> Maybe.map (.objectId >> Bullet.anyObjectId)
                |> Maybe.map (Url.NewBullet "collection-spread" "CollectionSpread")
                |> Maybe.map (Navigation.newUrl << Url.toString)
                |> Maybe.withDefault Cmd.none
            )



--        TitleChanged title ->
--            ( { model | title = title }, Cmd.none )
--
--        BulletChanged index input ->
--            let
--                numBullets =
--                    List.length model.bullets
--            in
--                (model.bullets
--                    |> \bullets ->
--                        if numBullets < index + 1 then
--                            bullets ++ List.repeat (index - numBullets + 1) (Bullet.Note { text = "" })
--                        else
--                            bullets
--                )
--                    |> List.indexedMap
--                        (\otherIndex bullet ->
--                            if otherIndex == index then
--                                Bullet.Note { text = input }
--                            else
--                                bullet
--                        )
--                    |> \bullets ->
--                        ( { model | bullets = bullets }, Cmd.none )


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
view lift viewConfig model =
    let
        collectionSpread_ =
            model.collectionSpread

        collectionSpread =
            collectionSpread_
                |> Maybe.map
                    CollectionSpread.fromParseObject
    in
        Html.div
            [ Html.class "collection-spread"
            ]
            [ viewConfig.toolbar
                { additionalSections =
                    [ Toolbar.section
                        [ Toolbar.alignEnd
                        ]
                        [ Button.view (lift << Mdc)
                            "collection-spread__new-bullet"
                            model.mdc
                            [ Button.ripple
                            , Button.onClick (lift NewBulletClicked)
                            ]
                            [ text "New bullet"
                            ]
                        ]
                    ]
                }
            , Html.div
                [ Html.class "collection-spread__title"
                ]
                [ text
                    (collectionSpread
                        |> Maybe.map CollectionSpread.title
                        |> Maybe.withDefault ""
                    )
                ]
            , Html.ol
                []
                (List.indexedMap
                    (\index bullet ->
                        Bullet.view
                            { node = Html.li
                            , additionalAttributes =
                                [ Html.class "collection-spread__bullet"
                                ]
                            }
                            (Bullet.fromParseObject bullet)
                    )
                    model.bullets
                )
            ]
