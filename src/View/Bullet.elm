module View.Bullet exposing (..)

import Html exposing (Html, text)
import Types.Bullet as Bullet exposing (Bullet)


view : (Msg -> msg) -> Bullet -> Html msg
view lift bullet =
    case bullet of
        Bullet.Task task ->
            viewTask lift task

        Bullet.Event event ->
            viewEvent lift event

        Bullet.Note note ->
            viewNote lift note


viewTask : (Msg -> msg) -> { text : String, state : TaskState } -> Html msg
viewTask lift task =
    text (toString task)


viewEvent : (Msg -> msg) -> { text : String } -> Html msg
viewEvent lift event =
    text (toString event)


viewNote : (Msg -> msg) -> { text : String } -> Html msg
viewNote lift note =
    text (toString note)
