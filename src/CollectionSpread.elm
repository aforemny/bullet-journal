module CollectionSpread exposing (..)

import Date exposing (Date)
import Html.Attributes as Html
import Html.Events as Html
import Html exposing (Html, text)


type alias Model =
    { id : Id
    , createdDate : Date
    , title : Title
    , bullets : List String
    }


empty : Id -> Date -> Model
empty id date =
    { id = id
    , createdDate = date
    , title = ""
    , bullets = []
    }


type alias Id =
    String


type alias Title =
    String


type Msg
    = TitleChanged String
    | BulletChanged Index String


type alias Index =
    Int


update : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update lift msg model =
    case msg of
        TitleChanged title ->
            ( { model | title = title }, Cmd.none )

        BulletChanged index input ->
            let
                numBullets =
                    List.length model.bullets
            in
                (model.bullets
                    |> \bullets ->
                        if numBullets < index + 1 then
                            bullets ++ List.repeat (index - numBullets + 1) ""
                        else
                            bullets
                )
                    |> List.indexedMap
                        (\otherIndex otherInput ->
                            if otherIndex == index then
                                input
                            else
                                otherInput
                        )
                    |> \bullets ->
                        ( { model | bullets = bullets }, Cmd.none )


view : (Msg -> msg) -> Model -> Html msg
view lift model =
    Html.div
        [ Html.class "collection-spread"
        ]
        [ Html.div
            [ Html.class "collection-spread__title"
            ]
            [ Html.input
                [ Html.value model.title
                , Html.onInput (lift << TitleChanged)
                ]
                []
            ]
        , Html.ol
            []
            (List.indexedMap
                (\index bullet ->
                    Html.li
                        []
                        [ Html.input
                            [ Html.value bullet
                            , Html.onInput (lift << BulletChanged index)
                            ]
                            []
                        ]
                )
                (model.bullets ++ [ "" ])
            )
        ]


title : Model -> String
title model =
    model.title
