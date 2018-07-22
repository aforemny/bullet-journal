port module Ports exposing (..)

import Date exposing (Date)
import Json.Decode exposing (Value)
import State exposing (State)
import Time.Calendar.Days as Calendar
import Time.Calendar.Gregorian as Calendar


port todayUnsafe : (String -> msg) -> Sub msg


today : (Calendar.Day -> msg) -> Sub msg
today lift =
    todayUnsafe (lift << readDayUnsafe)


readDayUnsafe : String -> Calendar.Day
readDayUnsafe dayString =
    case
        dayString
            |> String.split "-"
            |> List.map String.toInt
    of
        [ Ok year, Ok month, Ok dayOfMonth ] ->
            Calendar.fromGregorian year month dayOfMonth

        _ ->
            Debug.crash ("Ports.readDayUnsafe: " ++ dayString)


port nowUnsafe : (String -> msg) -> Sub msg


now : (Date -> msg) -> Sub msg
now lift =
    nowUnsafe (lift << readDateUnsafe)


readDateUnsafe : String -> Date
readDateUnsafe dateString =
    case Date.fromString dateString of
        Ok date ->
            date

        Err _ ->
            Debug.crash ("Ports.readDateUnsafe: " ++ dateString)


port persistUnsafe : Value -> Cmd msg


persist : State -> Cmd msg
persist state =
    persistUnsafe (State.encode state)
