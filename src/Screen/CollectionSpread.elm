module Screen.CollectionSpread exposing
    ( Config
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Material.Button exposing (buttonConfig, textButton)
import Material.Card exposing (card, cardBlock, cardConfig)
import Material.IconButton exposing (iconButton, iconButtonConfig)
import Material.List exposing (list, listConfig)
import Material.TopAppBar as TopAppBar
import Parse
import Parse.Private.ObjectId as ObjectId
import Route
import Task exposing (Task)
import Time
import Time.Calendar.Days as Calendar
import Type.Bullet as Bullet exposing (Bullet)
import Type.CollectionSpread as CollectionSpread exposing (CollectionSpread)


type alias Config msg =
    { key : Browser.Navigation.Key
    , today : Calendar.Day
    , parse : Parse.Config
    , topAppBar :
        { title : String
        , menuIcon : Maybe (Html msg)
        , additionalSections : List (Html msg)
        }
        -> Html msg
    , fixedAdjust : Html.Attribute msg
    }


type alias Model =
    { collectionSpread : Maybe (Parse.Object CollectionSpread)
    , bullets : List (Parse.Object Bullet)
    , error : Maybe Parse.Error
    }


defaultModel : Model
defaultModel =
    { collectionSpread = Nothing
    , bullets = []
    , error = Nothing
    }


type Msg msg
    = CollectionSpreadResult (Result Parse.Error (Parse.Object CollectionSpread))
    | BulletsResult (Result Parse.Error (List (Parse.Object Bullet)))
    | NewBulletClicked
    | EditClicked
    | BackClicked
    | BulletClicked (Parse.ObjectId Bullet)


init :
    (Msg msg -> msg)
    -> Config msg
    -> Parse.ObjectId CollectionSpread
    -> ( Model, Cmd msg )
init lift config objectId =
    ( defaultModel
    , Cmd.batch
        [ Task.attempt (lift << CollectionSpreadResult)
            (CollectionSpread.get config.parse objectId)
        , Parse.send config.parse
            (lift << BulletsResult)
            (Parse.query Bullet.decode (Parse.emptyQuery "Bullet"))
        ]
    )


subscriptions : (Msg msg -> msg) -> Model -> Sub msg
subscriptions lift model =
    Sub.none


update :
    (Msg msg -> msg)
    -> Config msg
    -> Msg msg
    -> Model
    -> ( Model, Cmd msg )
update lift config msg model =
    case msg of
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
            , Browser.Navigation.pushUrl config.key
                (Route.toString (Route.EditBullet Nothing))
            )

        EditClicked ->
            ( model
            , model.collectionSpread
                |> Maybe.map (Route.EditCollectionSpread << Just << .objectId)
                |> Maybe.map (Browser.Navigation.pushUrl config.key << Route.toString)
                |> Maybe.withDefault Cmd.none
            )

        BulletClicked bulletId ->
            ( model
            , Browser.Navigation.pushUrl config.key
                (Route.toString (Route.EditBullet (Just bulletId)))
            )

        BackClicked ->
            ( model
            , Browser.Navigation.pushUrl config.key (Route.toString Route.TableOfContent)
            )


view : (Msg msg -> msg) -> Config msg -> Model -> List (Html msg)
view lift config model =
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
    [ config.topAppBar
        { title = title
        , menuIcon =
            Just <|
                iconButton
                    { iconButtonConfig
                        | onClick = Just (lift BackClicked)
                        , additionalAttributes = [ TopAppBar.navigationIcon ]
                    }
                    "arrow_back"
        , additionalSections =
            [ TopAppBar.section [ TopAppBar.alignEnd ]
                [ textButton
                    { buttonConfig
                        | onClick = Just (lift EditClicked)
                    }
                    "Edit"
                , textButton
                    { buttonConfig
                        | onClick = Just (lift NewBulletClicked)
                    }
                    "New bullet"
                ]
            ]
        }
    , Html.div
        [ class "collection-spread"
        ]
        [ card
            { cardConfig
                | additionalAttributes =
                    [ class "collection-spread__wrapper" ]
            }
            { blocks =
                [ cardBlock <|
                    Html.div []
                        [ Html.div
                            [ class "collection-spread__primary" ]
                            [ Html.div
                                [ class "collection-spread__title" ]
                                [ text title ]
                            , Html.div
                                [ class "collection-spread__subtitle" ]
                                [ text "Describe collection" ]
                            ]
                        , if List.isEmpty model.bullets then
                            text ""

                          else
                            list
                                { listConfig
                                    | additionalAttributes =
                                        [ class "collection-spread__bullet-wrapper" ]
                                }
                                (List.map
                                    (\bullet ->
                                        Bullet.view
                                            { additionalOptions =
                                                [ class "collection-spread__bullet"
                                                , Html.Events.onClick (lift (BulletClicked bullet.objectId))
                                                ]
                                            }
                                            (Bullet.fromParseObject bullet)
                                    )
                                    model.bullets
                                )
                        ]
                ]
            , actions = Nothing
            }
        ]
    ]
