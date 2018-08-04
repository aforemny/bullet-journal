module ParseConfig exposing (parseConfig)

import Parse


parseConfig : Parse.Config
parseConfig =
    Parse.simpleConfig "bujo" "bujo"
        |> (\config ->
                { config | serverUrl = "http://localhost:1337/parse" }
           )
