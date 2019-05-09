module View.CollectionSpread exposing (Model, Msg(..), defaultModel, init, subscriptions, update, view)

import Browser.Navigation
import Html exposing (Html, text)
import Html.Attributes as Html
import Html.Events as Html
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Icon as Icon
import Material.List as Lists
import Material.Options as Options exposing (cs, css, styled, when)
import Material.Toolbar as Toolbar
import Parse
import Parse.Private.ObjectId as ObjectId
import Route
import Task exposing (Task)
import Time
import Type.Bullet as Bullet exposing (Bullet)
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)
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
    | EditClicked
    | BackClicked
    | BulletClicked (Parse.ObjectId Bullet)


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
            (Bullet.getOf viewConfig.parse "CollectionSpread" objectId)
        ]
    )


subscriptions : (Msg msg -> msg) -> Model msg -> Sub msg
subscriptions lift model =
    Material.subscriptions (lift << Mdc) model


update :
    (Msg msg -> msg)
    -> View.Config msg
    -> Msg msg
    -> Model msg
    -> ( Model msg, Cmd msg )
update lift viewConfig msg model =
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
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.EditBullet Nothing))
            )

        EditClicked ->
            ( model
            , model.collectionSpread
                |> Maybe.map (Route.EditCollectionSpread << .objectId)
                |> Maybe.map (Browser.Navigation.pushUrl viewConfig.key << Route.toString)
                |> Maybe.withDefault Cmd.none
            )

        BulletClicked bulletId ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key
                (Route.toString (Route.EditBullet (Just bulletId)))
            )

        BackClicked ->
            ( model
            , Browser.Navigation.pushUrl viewConfig.key (Route.toString Route.Index)
            )


view : (Msg msg -> msg) -> View.Config msg -> Model msg -> Html msg
view lift viewConfig model =
    let
        collectionSpread_ =
            model.collectionSpread

        collectionSpread =
            collectionSpread_
                |> Maybe.map
                    CollectionSpread.fromParseObject

        title =
            collectionSpread
                |> Maybe.map CollectionSpread.title
                |> Maybe.withDefault "Collection"
    in
    Html.div
        [ Html.class "collection-spread"
        ]
        [ viewConfig.toolbar
            { title =
                title
            , menuIcon =
                Icon.view
                    [ Toolbar.menuIcon
                    , Options.onClick (lift BackClicked)
                    ]
                    "arrow_back"
            , additionalSections =
                [ Toolbar.section
                    [ Toolbar.alignEnd
                    ]
                    [ Button.view (lift << Mdc)
                        "collection-spread__edit-collection-spread"
                        model.mdc
                        [ Button.ripple
                        , Button.onClick (lift EditClicked)
                        ]
                        [ text "Edit"
                        ]
                    , Button.view (lift << Mdc)
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
        , Card.view
            [ cs "collection-spread__wrapper" ]
            [ Html.div
                [ Html.class "collection-spread__primary" ]
                [ Html.div
                    [ Html.class "collection-spread__title" ]
                    [ text title ]
                , Html.div
                    [ Html.class "collection-spread__subtitle" ]
                    [ text "Describe collection" ]
                ]
            , Lists.ol (lift << Mdc)
                -- TODO: id
                ""
                model.mdc
                [ cs "collection-spread__bullet-wrapper" ]
                (List.map
                    (\bullet ->
                        Bullet.view
                            { additionalOptions =
                                [ cs "collection-spread__bullet"
                                , Options.onClick (lift (BulletClicked bullet.objectId))
                                ]
                            }
                            (Bullet.fromParseObject bullet)
                    )
                    model.bullets
                )
            ]
        ]
