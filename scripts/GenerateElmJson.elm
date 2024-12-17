module GenerateElmJson exposing (run)

import BackendTask exposing (BackendTask)
import Cldr
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Cldr.allLocales
        |> List.map Cldr.toModuleName
        |> String.join "\n,  "
        |> Script.log
