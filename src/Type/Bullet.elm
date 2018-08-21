module Type.Bullet exposing (..)

import Date exposing (Date)
import Html exposing (Html, text)
import Html.Attributes as Html
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Material.List as Lists
import Material.Options as Options exposing (cs, css, styled, when)
import Parse
import Parse.Decode
import Parse.Encode
import Private.ObjectId as ObjectId
import Private.Pointer as Pointer
import Task exposing (Task)


type alias Bullet =
    { spreadClass : Maybe String
    , spreadId : Maybe (Parse.ObjectId Any)
    , state : State
    , text : String
    , year : Maybe Int
    , month : Maybe Int
    , dayOfMonth : Maybe Int
    }


empty : Bullet
empty =
    emptyNote


emptyNote : Bullet
emptyNote =
    { spreadClass = Nothing
    , spreadId = Nothing
    , state = Note
    , text = ""
    , year = Nothing
    , month = Nothing
    , dayOfMonth = Nothing
    }


emptyTask : Bullet
emptyTask =
    { spreadClass = Nothing
    , spreadId = Nothing
    , state = Task Unchecked
    , text = ""
    , year = Nothing
    , month = Nothing
    , dayOfMonth = Nothing
    }


emptyEvent : Bullet
emptyEvent =
    { spreadClass = Nothing
    , spreadId = Nothing
    , state = Event
    , text = ""
    , year = Nothing
    , month = Nothing
    , dayOfMonth = Nothing
    }


fromParseObject : Parse.Object Bullet -> Bullet
fromParseObject bullet =
    { spreadClass = bullet.spreadClass
    , spreadId = bullet.spreadId
    , state = bullet.state
    , text = bullet.text
    , year = bullet.year
    , month = bullet.month
    , dayOfMonth = bullet.dayOfMonth
    }


encode : Bullet -> Value
encode bullet =
    Encode.object
        [ ( "spreadClass", Maybe.withDefault Encode.null (Maybe.map Encode.string bullet.spreadClass) )
        , ( "spreadId", Maybe.withDefault Encode.null (Maybe.map Parse.Encode.objectId bullet.spreadId) )
        , ( "state", encodeState bullet.state )
        , ( "text", Encode.string bullet.text )
        , ( "year", Maybe.withDefault Encode.null (Maybe.map Encode.int bullet.year) )
        , ( "month", Maybe.withDefault Encode.null (Maybe.map Encode.int bullet.month) )
        , ( "dayOfMonth", Maybe.withDefault Encode.null (Maybe.map Encode.int bullet.dayOfMonth) )
        ]


decode : Decoder (Parse.Object Bullet)
decode =
    Decode.succeed
        (\objectId createdAt updatedAt spreadClass spreadId state text year month dayOfMonth ->
            { objectId = objectId
            , createdAt = createdAt
            , updatedAt = updatedAt
            , spreadClass = spreadClass
            , spreadId = spreadId
            , state = state
            , text = text
            , year = year
            , month = month
            , dayOfMonth = dayOfMonth
            }
        )
        |> Decode.required "objectId" Parse.Decode.objectId
        |> Decode.required "createdAt" Parse.Decode.date
        |> Decode.required "updatedAt" Parse.Decode.date
        |> Decode.optional "spreadClass" (Decode.map Just Decode.string) Nothing
        |> Decode.optional "spreadId" (Decode.map Just Parse.Decode.objectId) Nothing
        |> Decode.required "state" decodeState
        |> Decode.required "text" Decode.string
        |> Decode.optional "year" (Decode.map Just Decode.int) Nothing
        |> Decode.optional "month" (Decode.map Just Decode.int) Nothing
        |> Decode.optional "dayOfMonth" (Decode.map Just Decode.int) Nothing


type Any
    = Any


anyObjectId : Parse.ObjectId a -> Parse.ObjectId Any
anyObjectId =
    ObjectId.toString >> ObjectId.fromString


castObjectId : Parse.ObjectId Any -> Parse.ObjectId b
castObjectId =
    ObjectId.toString >> ObjectId.fromString


type State
    = Event
    | Note
    | Task TaskState


encodeState : State -> Value
encodeState state =
    case state of
        Event ->
            Encode.object [ ( "ctor", Encode.string "event" ) ]

        Note ->
            Encode.object [ ( "ctor", Encode.string "note" ) ]

        Task taskState ->
            Encode.object
                [ ( "ctor", Encode.string "task" )
                , ( "taskState", encodeTaskState taskState )
                ]


decodeState : Decoder State
decodeState =
    let
        decodeEvent =
            Decode.succeed Event

        decodeNote =
            Decode.succeed Note

        decodeTask =
            Decode.map Task (Decode.at [ "taskState" ] decodeTaskState)
    in
    Decode.at [ "ctor" ] Decode.string
        |> Decode.andThen
            (\ctor ->
                case ctor of
                    "event" ->
                        decodeEvent

                    "note" ->
                        decodeNote

                    "task" ->
                        decodeTask

                    _ ->
                        Decode.fail ("expected event, note or task but got " ++ ctor)
            )


type TaskState
    = Unchecked
    | Checked
    | Migrated


encodeTaskState : TaskState -> Value
encodeTaskState taskState =
    Encode.string <|
        case taskState of
            Unchecked ->
                "unchecked"

            Checked ->
                "checked"

            Migrated ->
                "migrated"


decodeTaskState : Decoder TaskState
decodeTaskState =
    Decode.string
        |> Decode.andThen
            (\taskStateString ->
                case taskStateString of
                    "unchecked" ->
                        Decode.succeed Unchecked

                    "checked" ->
                        Decode.succeed Checked

                    "migrated" ->
                        Decode.succeed Migrated

                    _ ->
                        Decode.fail ("expected unchecked, checked or migrated, but got " ++ taskStateString)
            )


get :
    Parse.Config
    -> Parse.ObjectId Bullet
    -> Task Parse.Error (Parse.Object Bullet)
get parse objectId =
    Parse.toTask parse (Parse.get "Bullet" decode objectId)


getOf :
    Parse.Config
    -> String
    -> Parse.ObjectId spreadId
    -> Task Parse.Error (List (Parse.Object Bullet))
getOf parse spreadClass spreadId =
    Parse.toTask parse
        (Parse.query decode
            (Parse.emptyQuery "Bullet"
                |> (\query ->
                        { query
                            | whereClause =
                                Parse.and
                                    [ Parse.equalTo "spreadClass" (Encode.string spreadClass)
                                    , Parse.equalTo "spreadId" (Parse.Encode.objectId spreadId)
                                    ]
                        }
                   )
            )
        )


create :
    Parse.Config
    -> Bullet
    -> Task Parse.Error { objectId : Parse.ObjectId Bullet, createdAt : Date }
create parse bullet =
    Parse.toTask parse (Parse.create "Bullet" encode bullet)


update :
    Parse.Config
    -> Parse.ObjectId Bullet
    -> Bullet
    -> Task Parse.Error { updatedAt : Date }
update parse bulletId bullet =
    Parse.toTask parse (Parse.update "Bullet" encode bulletId bullet)


delete :
    Parse.Config
    -> Parse.ObjectId Bullet
    -> Task Parse.Error {}
delete parse bulletId =
    Parse.toTask parse (Parse.delete "Bullet" bulletId)


type alias Config msg =
    { additionalOptions : List (Lists.Property msg)
    }


view : Config msg -> Bullet -> Html msg
view config bullet =
    Lists.li
        (cs "bullet"
            :: (case bullet.state of
                    Event ->
                        cs "bullet--event"

                    Note ->
                        cs "bullet--note"

                    Task Unchecked ->
                        cs "bullet--task bullet--task--unchecked"

                    Task Checked ->
                        cs "bullet--task bullet--task--checked"

                    Task Migrated ->
                        cs "bullet--task bullet--task--migrated"
               )
            :: config.additionalOptions
        )
        [ Lists.graphicIcon
            []
            (case bullet.state of
                Event ->
                    "radio_button_unchecked"

                Note ->
                    "indeterminate_check_box"

                Task Unchecked ->
                    "check_box_outline_blank"

                Task Checked ->
                    "check_box"

                Task Migrated ->
                    "check_box"
            )
        , Lists.text []
            [ text bullet.text
            ]
        ]
