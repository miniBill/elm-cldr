module Generate exposing (main)

{-| -}

import Dict exposing (Dict)
import Elm
import Elm.Annotation as Annotation exposing (Annotation)
import Elm.Case
import Elm.Case.Branch
import Gen.CodeGen.Generate as Generate exposing (Directory(..))
import Iso3166
import Json.Decode
import Json.Encode
import Maybe.Extra
import Set exposing (Set)
import String.Extra


main : Program Json.Encode.Value () ()
main =
    Generate.fromDirectory
        (\directory ->
            case getEnglishData directory of
                Err e ->
                    [ error "Error" e ]

                Ok ( languagesEnglishDict, territoriesEnglishDict ) ->
                    let
                        languageNames : Set String
                        languageNames =
                            Dict.values languagesEnglishDict
                                |> Set.fromList
                                |> Set.insert "Pidgin"
                                |> Set.insert "Gaelic"

                        shared =
                            { languageNames = languageNames
                            , languagesEnglishDict = languagesEnglishDict
                            , territoriesEnglishDict = territoriesEnglishDict
                            }
                    in
                    mainFile directory shared
                        ++ files directory shared
        )


type alias Shared =
    { languageNames : Set String
    , languagesEnglishDict : Dict String String
    , territoriesEnglishDict : Dict String String
    }


mainFile : Directory -> Shared -> List Elm.File
mainFile (Directory directory) shared =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.named [] "CountryCode"

        localeAnnotation : Annotation
        localeAnnotation =
            Annotation.named [] "Locale"

        allLocales : List { key : String, name : String, moduleName : List String, variant : String }
        allLocales =
            directory.directories
                |> Dict.keys
                |> List.filterMap
                    (\key ->
                        getLanguageName shared key
                            |> Maybe.map
                                (\( name, moduleName ) ->
                                    { key = key
                                    , name = name
                                    , moduleName = moduleName
                                    , variant =
                                        key
                                            |> String.split "-"
                                            |> List.map String.Extra.toSentenceCase
                                            |> String.concat
                                    }
                                )
                    )
    in
    [ Elm.file [ "Cldr", "Localized" ]
        [ (\locale countryCode ->
            allLocales
                |> List.map
                    (\{ variant, moduleName } ->
                        Elm.Case.branch0 variant
                            (Elm.apply
                                (Elm.value
                                    { importFrom = "Cldr" :: moduleName
                                    , name = "countryCodeToName"
                                    , annotation =
                                        Just <|
                                            Annotation.function
                                                [ Annotation.named [ "Cldr" ] "CountryCode" ]
                                                Annotation.string
                                    }
                                )
                                [ countryCode ]
                            )
                    )
                |> Elm.Case.custom locale (Annotation.named [ "Cldr" ] "Locale")
          )
            |> Elm.fn2
                ( "locale", Just <| Annotation.named [ "Cldr" ] "Locale" )
                ( "countryCode", Just <| Annotation.named [ "Cldr" ] "CountryCode" )
            |> Elm.declaration "countryCodeToName"
            |> Elm.expose
        ]
    , Elm.file [ "Cldr" ]
        [ allCountryCodes
            |> List.map Elm.variant
            |> Elm.customType "CountryCode"
            |> Elm.withDocumentation "All the supported country codes. `GT` and `LT` are defined in Basics so we define them as `GT_` and `LT_`."
            |> Elm.exposeWith { exposeConstructor = True, group = Nothing }
        , allLocales
            |> List.map (\{ variant } -> Elm.variant variant)
            |> Elm.customType "Locale"
            |> Elm.withDocumentation "All the supported locales."
            |> Elm.exposeWith { exposeConstructor = True, group = Nothing }
        , allLocales
            |> List.map (\{ variant } -> Elm.withType localeAnnotation <| Elm.val variant)
            |> Elm.list
            |> Elm.declaration "allLocales"
            |> Elm.withDocumentation "All the supported locales."
            |> Elm.expose
        , (\locale ->
            allLocales
                |> List.map (\{ variant, name } -> Elm.Case.branch0 variant (Elm.string name))
                |> Elm.Case.custom locale localeAnnotation
          )
            |> Elm.fn ( "locale", Just localeAnnotation )
            |> Elm.declaration "localeToName"
            |> Elm.withDocumentation "Get the name of a locale."
            |> Elm.expose
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
                |> Elm.Case.custom countryCodeExpr countryCodeAnnotation
          )
            |> Elm.fn ( "countryCodeExpr", Just countryCodeAnnotation )
            |> Elm.declaration "toAlpha2"
            |> Elm.withDocumentation "Two-letter `ISO 3166-1 alpha-2` code from `CountryCode`."
            |> Elm.expose
        , allCountryCodes
            |> List.map (\countryCode -> Elm.val countryCode)
            |> Elm.list
            |> Elm.withType (Annotation.list countryCodeAnnotation)
            |> Elm.declaration "all"
            |> Elm.withDocumentation "All `CountryCode`s sorted alphabetically."
            |> Elm.expose
        ]
    ]


files : Directory -> Shared -> List Elm.File
files ((Directory dir) as directory) ({ languagesEnglishDict } as shared) =
    let
        ( allDictionaries, allErrors ) =
            dir.directories
                |> Dict.keys
                |> List.foldl
                    (\key ( dictAcc, errAcc ) ->
                        if key == "und" then
                            -- Unknown language
                            ( dictAcc, errAcc )

                        else
                            case getLanguageName shared key of
                                Just ( name, moduleName ) ->
                                    case getTerritories key directory of
                                        Ok territories ->
                                            ( Dict.insert moduleName
                                                { name = name
                                                , territories = territories
                                                }
                                                dictAcc
                                            , errAcc
                                            )

                                        Err e ->
                                            ( dictAcc, error name e :: errAcc )

                                Nothing ->
                                    ( dictAcc
                                    , error key
                                        ("Failed to get language name, or split it, language name is "
                                            ++ Maybe.withDefault "Nothing" (Dict.get key languagesEnglishDict)
                                        )
                                        :: errAcc
                                    )
                    )
                    ( Dict.empty, [] )

        allFiles : List Elm.File
        allFiles =
            allDictionaries
                |> Dict.toList
                |> List.map
                    (\( moduleName, { name, territories } ) ->
                        let
                            parentModule : List String
                            parentModule =
                                case moduleName of
                                    [ "Spanish", "Argentina" ] ->
                                        [ "Spanish" ]

                                    [ "Spanish", _ ] ->
                                        [ "Spanish", "Argentina" ]

                                    [ "English", "UnitedKingdom" ] ->
                                        [ "English" ]

                                    [ "English", _ ] ->
                                        if Dict.get "MF" territories == Just "St. Martin" then
                                            [ "English" ]

                                        else
                                            [ "English", "UnitedKingdom" ]

                                    [ "Portuguese", "Portugal" ] ->
                                        [ "Portuguese" ]

                                    [ "Portuguese", _ ] ->
                                        [ "Portuguese", "Portugal" ]

                                    _ ->
                                        moduleName
                                            |> List.reverse
                                            |> List.drop 1
                                            |> List.reverse

                            parent :
                                { name : String
                                , territories : Dict String String
                                }
                            parent =
                                allDictionaries
                                    |> Dict.get parentModule
                                    |> Maybe.withDefault
                                        { name = ""
                                        , territories = Dict.empty
                                        }
                        in
                        Elm.file ("Cldr" :: moduleName)
                            [ countryCodeToNameDeclaration
                                { name = parent.name
                                , moduleName = parentModule
                                , territories = parent.territories
                                }
                                name
                                territories
                            ]
                    )
    in
    allErrors ++ allFiles


getLanguageName : Shared -> String -> Maybe ( String, List String )
getLanguageName { languageNames, languagesEnglishDict, territoriesEnglishDict } key =
    if key == "und" then
        Nothing

    else
        let
            standard : String -> Maybe ( String, List String )
            standard needle =
                Dict.get needle languagesEnglishDict
                    |> Maybe.andThen
                        (\languageName ->
                            Maybe.map
                                (Tuple.pair languageName)
                                (splitLanguage languageNames languageName)
                        )

            withSuffix : String -> String -> Maybe ( String, List String )
            withSuffix suffix prefix =
                Maybe.map
                    (\( languageName, languageSplit ) ->
                        ( languageName ++ " (" ++ suffix ++ ")"
                        , languageSplit ++ [ suffix ]
                        )
                    )
                    (standard prefix)

            checkSuffix : String -> Maybe ( String, List String )
            checkSuffix needle =
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

                    _ ->
                        standard needle

            tryLanguageAndTerritory : String -> String -> Maybe ( String, List String )
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
                        (checkSuffix language)
                        (Dict.get territory territoriesEnglishDict)
                    )
                    (\_ -> checkSuffix key)
        in
        case String.split "-" key of
            [ "ca", "ES", "valencia" ] ->
                Just
                    ( "Catalan - Valencia"
                    , [ "Catalan", "Valencia" ]
                    )

            [ _ ] ->
                standard key

            [ prefix, suffix ] ->
                tryLanguageAndTerritory prefix suffix

            [ prefix1, prefix2, suffix ] ->
                tryLanguageAndTerritory (prefix1 ++ "-" ++ prefix2) suffix

            _ ->
                Nothing


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


countryCodeToNameDeclaration :
    { name : String
    , moduleName : List String
    , territories : Dict String String
    }
    -> String
    -> Dict String String
    -> Elm.Declaration
countryCodeToNameDeclaration parent languageName territories =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.namedWith [ "Cldr" ] "CountryCode" []

        parentFunction =
            Elm.value
                { importFrom = "Cldr" :: parent.moduleName
                , name = "countryCodeToName"
                , annotation = Just <| Annotation.function [ countryCodeAnnotation ] Annotation.string
                }

        branches : List Elm.Case.Branch
        branches =
            allCountryCodes
                |> List.filterMap
                    (\countryCode ->
                        let
                            countryCodeClean : String
                            countryCodeClean =
                                String.replace "_" "" countryCode
                        in
                        Dict.get countryCodeClean territories
                            |> Maybe.andThen
                                (\name ->
                                    if Just name == Dict.get countryCodeClean parent.territories then
                                        Nothing

                                    else
                                        Just <| Elm.Case.branch0 countryCode (Elm.string name)
                                )
                    )

        table : String
        table =
            allCountryCodes
                |> List.filterMap
                    (\countryCode ->
                        let
                            countryCodeClean : String
                            countryCodeClean =
                                String.replace "_" "" countryCode
                        in
                        Dict.get countryCodeClean territories
                            |> Maybe.map (\name -> "    " ++ countryCodeClean ++ " " ++ name)
                    )
                |> String.join "\n"
    in
    if List.isEmpty branches then
        parentFunction
            |> Elm.declaration "countryCodeToName"
            |> Elm.withDocumentation ("Name for `CountryCode` in " ++ languageName ++ ".\n\nThis is identical to the " ++ parent.name ++ " version.\n\n" ++ table)
            |> Elm.expose

    else
        Elm.fn ( "countryCode", Just countryCodeAnnotation )
            (\countryCodeExpr ->
                (if List.length branches == List.length allCountryCodes then
                    branches

                 else
                    branches
                        ++ [ Elm.Case.Branch.ignore
                                (Elm.apply parentFunction [ countryCodeExpr ])
                           ]
                )
                    |> Elm.Case.custom countryCodeExpr countryCodeAnnotation
            )
            |> Elm.declaration "countryCodeToName"
            |> Elm.withDocumentation ("Name for `CountryCode` in " ++ languageName ++ ".\n\n" ++ table)
            |> Elm.expose


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


error : String -> String -> Elm.File
error file msg =
    Elm.file [ file ]
        [ Elm.declaration "error" <|
            Elm.string msg
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
