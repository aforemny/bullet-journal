module Type.Bullet.Parser exposing (parse)

import Parser exposing (..)
import String
import Type.Bullet as Bullet exposing (Bullet, emptyEvent, emptyNote, emptyTask)


parse : String -> Bullet
parse input =
    run parser input
        |> Result.toMaybe
        |> Maybe.withDefault
            { emptyTask
                | text = input
            }


parser =
    oneOf
        [ eventParser
        , taskParser
        , noteParser
        ]


eventParser =
    succeed (\text -> { emptyEvent | text = text })
        |. symbol "O"
        |= textParser


taskParser =
    oneOf
        [ checkedTaskParser
        , uncheckedTaskParser
        ]


uncheckedTaskParser =
    succeed (\text -> { emptyTask | text = text })
        |. oneOf
            [ symbol "."
            , symbol "[]"
            , symbol "[ ]"
            ]
        |= textParser


checkedTaskParser =
    succeed
        (\text ->
            { emptyTask | text = text, state = Bullet.Task Bullet.Checked }
        )
        |. symbol "[x]"
        |= textParser


noteParser =
    succeed (\text -> { emptyNote | text = text })
        |. symbol "-"
        |= textParser


spaces =
    ignore zeroOrMore (\char -> List.member char [ ' ', '\t', '\x0D', '\n' ])


textParser =
    keep zeroOrMore (always True)
        |> map String.trim
