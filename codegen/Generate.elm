module Generate exposing (main)

{-| -}

import Dict exposing (Dict)
import Elm
import Elm.Annotation as Annotation exposing (Annotation)
import Elm.Case
import Gen.CodeGen.Generate as Generate exposing (Directory(..))
import Iso3166
import Json.Decode
import Json.Encode
import Maybe.Extra
import Set exposing (Set)
import String.Extra


main : Program Json.Encode.Value () ()
main =
    Generate.fromDirectory (\directory -> mainFile :: files directory)


mainFile : Elm.File
mainFile =
    let
        ann : Annotation
        ann =
            Annotation.named [] "CountryCode"
    in
    Elm.file [ "Cldr" ]
        [ allCountryCodes
            |> List.map Elm.variant
            |> Elm.customType "CountryCode"
            |> Elm.withDocumentation "All the supported country codes. `GT` and `LT` are defined in Basics so we define them as `GT_` and `LT_`."
            |> Elm.exposeWith { exposeConstructor = True, group = Nothing }
        , (\countryCodeExpr ->
            allCountryCodes
                |> List.map
                    (\countryCode ->
                        Elm.Case.branch0 countryCode
                            (countryCode
                                |> String.toLower
                                |> String.replace "_" ""
                                |> Elm.string
                            )
                    )
                |> Elm.Case.custom countryCodeExpr ann
          )
            |> Elm.fn ( "countryCodeExpr", Just ann )
            |> Elm.declaration "toAlpha2"
            |> Elm.withDocumentation "Two-letter `ISO 3166-1 alpha-2` code from `CountryCode`."
            |> Elm.expose
        , allCountryCodes
            |> List.map (\countryCode -> Elm.val countryCode)
            |> Elm.list
            |> Elm.withType (Annotation.list ann)
            |> Elm.declaration "all"
            |> Elm.withDocumentation "All `CountryCode`s sorted alphabetically."
            |> Elm.expose
        ]


files : Generate.Directory -> List Elm.File
files ((Directory dir) as directory) =
    case getEnglishData directory of
        Err e ->
            error "Error" e

        Ok ( languagesEnglishDict, territoriesEnglishDict ) ->
            let
                languageNames : Set String
                languageNames =
                    Dict.values languagesEnglishDict
                        |> Set.fromList
                        |> Set.insert "Pidgin"
                        |> Set.insert "Gaelic"
            in
            dir.directories
                |> Dict.keys
                |> List.concatMap
                    (\key ->
                        if key == "und" then
                            -- Unknown language
                            []

                        else
                            let
                                withSuffix suffix prefix =
                                    Maybe.map
                                        (\( languageName, languageSplit ) ->
                                            ( languageName ++ " (" ++ suffix ++ ")"
                                            , languageSplit ++ [ suffix ]
                                            )
                                        )
                                        (standard prefix)

                                standard : String -> Maybe ( String, List String )
                                standard needle =
                                    case String.split "-" needle of
                                        [ prefix, "Guru" ] ->
                                            withSuffix "Gurmukhi" prefix

                                        [ prefix, "Hans" ] ->
                                            withSuffix "Simplified" prefix

                                        [ prefix, "Hant" ] ->
                                            withSuffix "Traditional" prefix

                                        [ prefix, "Latn" ] ->
                                            withSuffix "Latin" prefix

                                        [ prefix, "Arab" ] ->
                                            withSuffix "Arabic" prefix

                                        [ prefix, "tarask" ] ->
                                            withSuffix "Taraškievica" prefix

                                        [ prefix, "Cyrl" ] ->
                                            withSuffix "Cyrillic" prefix

                                        [ prefix, "polyton" ] ->
                                            withSuffix "Polytonic" prefix

                                        [ "ca", "ES", "valencia" ] ->
                                            Just
                                                ( "Catalan (Spain, Valencian)"
                                                , [ "Catalan", "Spain", "Valencia" ]
                                                )

                                        _ ->
                                            Dict.get needle languagesEnglishDict
                                                |> Maybe.andThen
                                                    (\languageName ->
                                                        Maybe.map
                                                            (Tuple.pair languageName)
                                                            (splitLanguage languageNames languageName)
                                                    )

                                tryLanguageAndTerritory language territory =
                                    Maybe.Extra.orLazy
                                        (Maybe.map2
                                            (\( languageName, languageSplit ) territoryName ->
                                                ( languageName ++ " - " ++ territoryName
                                                , languageSplit
                                                    ++ [ String.replace " " "" <|
                                                            cleanName territoryName
                                                       ]
                                                )
                                            )
                                            (standard language)
                                            (Dict.get territory territoriesEnglishDict)
                                        )
                                        (\_ -> standard key)

                                maybeLanguageName : Maybe ( String, List String )
                                maybeLanguageName =
                                    case String.split "-" key of
                                        [ _ ] ->
                                            standard key

                                        [ prefix, suffix ] ->
                                            tryLanguageAndTerritory prefix suffix

                                        [ prefix1, prefix2, suffix ] ->
                                            tryLanguageAndTerritory (prefix1 ++ "-" ++ prefix2) suffix

                                        _ ->
                                            Nothing
                            in
                            case maybeLanguageName of
                                Just ( languageName, splatLanguageName ) ->
                                    case getTerritories key directory of
                                        Ok territories ->
                                            [ Elm.file ("Cldr" :: splatLanguageName)
                                                [ countryCodeToNameDeclaration languageName territories
                                                ]
                                            ]

                                        Err e ->
                                            error languageName e

                                Nothing ->
                                    error key <|
                                        "Failed to get language name, or split it, language name is "
                                            ++ Maybe.withDefault "Nothing" (Dict.get key languagesEnglishDict)
                    )


getEnglishData : Directory -> Result String ( Dict String String, Dict String String )
getEnglishData directory =
    Result.map2 Tuple.pair
        (getLanguages "en" directory)
        (getTerritories "en" directory)


getTerritories : String -> Directory -> Result String (Dict String String)
getTerritories key (Directory directory) =
    case Dict.get key directory.directories of
        Just (Directory subdirectory) ->
            case Dict.get "territories.json" subdirectory.files of
                Just territoriesJson ->
                    Result.mapError (\e -> "\"territories.json\": decoding failed: " ++ Json.Decode.errorToString e) <| decodeTerritories key territoriesJson

                Nothing ->
                    Err "Could not find \"territories.json\""

        Nothing ->
            Err <| "Could not find directory \"" ++ key ++ "\""


getLanguages : String -> Directory -> Result String (Dict String String)
getLanguages key (Directory directory) =
    case Dict.get key directory.directories of
        Just (Directory subdirectory) ->
            case Dict.get "languages.json" subdirectory.files of
                Just languagesJson ->
                    Result.mapError (\e -> "\"languages.json\": decoding failed: " ++ Json.Decode.errorToString e) <| decodeLanguages languagesJson

                Nothing ->
                    Err "Could not find \"languages.json\""

        Nothing ->
            Err <| "Could not find directory \"" ++ key ++ "\""


decodeTerritories : String -> String -> Result Json.Decode.Error (Dict String String)
decodeTerritories key input =
    let
        decoder =
            Json.Decode.at
                [ "main"
                , key
                , "localeDisplayNames"
                , "territories"
                ]
                (Json.Decode.dict Json.Decode.string)
    in
    Json.Decode.decodeString decoder input


allCountryCodes : List String
allCountryCodes =
    Iso3166.all
        |> List.map (Iso3166.toAlpha2 >> toUpper)
        |> (::) "XK"
        -- Kosovo
        |> List.sort


toUpper : String -> String
toUpper input =
    input
        |> String.toUpper
        |> String.replace "GT" "GT_"
        |> String.replace "LT" "LT_"


countryCodeToNameDeclaration : String -> Dict String String -> Elm.Declaration
countryCodeToNameDeclaration languageName territories =
    Elm.fn ( "countryCode", Just countryCodeAnnotation )
        (\countryCodeExpr ->
            allCountryCodes
                |> List.filterMap
                    (\countryCode ->
                        Dict.get (String.replace "_" "" countryCode) territories
                            |> Maybe.map
                                (\name ->
                                    Elm.Case.branch0 countryCode (Elm.string name)
                                )
                    )
                |> Elm.Case.custom countryCodeExpr countryCodeAnnotation
        )
        |> Elm.declaration "countryCodeToName"
        |> Elm.withDocumentation ("Name for `CountryCode` in " ++ languageName ++ ".")
        |> Elm.expose


countryCodeAnnotation : Annotation
countryCodeAnnotation =
    Annotation.namedWith [ "Cldr" ] "CountryCode" []


cleanName : String -> String
cleanName name =
    name
        |> String.replace "." ""
        |> String.replace "&" "And"
        |> String.replace "-" ""
        |> String.replace "’" ""
        |> String.replace "(" ""
        |> String.replace ")" ""
        |> String.Extra.toSentenceCase


splitLanguage : Set String -> String -> Maybe (List String)
splitLanguage languagesNames lang =
    case
        cleanName lang
            |> String.split " "
            |> List.reverse
    of
        [ atom ] ->
            Just [ atom ]

        [ "(Latin)", "Hindi" ] ->
            Just [ "Hindi", "Latin" ]

        [ prefix, suffix ] ->
            if Set.member suffix languagesNames then
                Just [ suffix, prefix ]

            else if Set.member prefix languagesNames then
                Just [ prefix, suffix ]

            else
                Nothing

        head :: tail ->
            if Set.member head languagesNames then
                Just [ head, String.concat <| List.reverse tail ]

            else
                Nothing

        _ ->
            Nothing


error : String -> String -> List Elm.File
error file msg =
    [ Elm.file [ file ]
        [ Elm.declaration "error" <|
            Elm.string msg
        ]
    ]


decodeLanguages : String -> Result Json.Decode.Error (Dict String String)
decodeLanguages input =
    let
        decoder =
            Json.Decode.at
                [ "main"
                , "en"
                , "localeDisplayNames"
                , "languages"
                ]
                (Json.Decode.dict Json.Decode.string)
    in
    input
        |> Json.Decode.decodeString decoder
