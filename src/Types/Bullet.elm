module Types.Bullet exposing (..)

import Html.Attributes as Html
import Html exposing (Html, text)


type Bullet
    = Task
        { text : String
        , state : TaskState
        }
    | Event
        { text : String
        }
    | Note
        { text : String
        }


type TaskState
    = Unchecked
    | Checked
    | Migrated


type alias Config msg =
    { node : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , additionalAttributes : List (Html.Attribute msg)
    }


view : Config msg -> Bullet -> Html msg
view config bullet =
    config.node
        (Html.class "bullet"
            :: config.additionalAttributes
        )
        [ case bullet of
            Task task ->
                viewTask task

            Event event ->
                viewEvent event

            Note note ->
                viewNote note
        ]


viewTask : { text : String, state : TaskState } -> Html msg
viewTask task =
    text task.text


viewEvent : { text : String } -> Html msg
viewEvent event =
    text event.text


viewNote : { text : String } -> Html msg
viewNote note =
    text note.text
