module Generate exposing (run)

{-| -}

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Glob as Glob
import Cli.Option
import Cli.OptionsParser
import Cli.Program as Program
import Dict exposing (Dict)
import Elm
import Elm.Annotation as Annotation exposing (Annotation)
import Elm.Arg
import Elm.Case
import FatalError exposing (FatalError)
import Gen.Debug
import Gen.Maybe
import Gen.String
import Iso3166
import Json.Decode exposing (Decoder)
import Json.Encode
import LanguageTag
import LanguageTag.ExtendedLanguage as ExtendedLanguage
import LanguageTag.Language as Language
import LanguageTag.Parser
import LanguageTag.PrivateUse as PrivateUse
import LanguageTag.Region as Region exposing (Region)
import LanguageTag.Script as Script exposing (Script)
import LanguageTag.Variant as Variant exposing (Variant)
import List.Extra
import List.Nonempty
import Maybe.Extra
import Pages.Script
import Result.Extra
import ResultME exposing (ResultME)
import Set exposing (Set)
import String.Extra


run : Pages.Script.Script
run =
    Pages.Script.withCliOptions config toTask


type alias Config =
    { flagsFrom : String
    , output : String
    }


config : Program.Config Config
config =
    Program.config
        |> Program.add
            (Cli.OptionsParser.build Config
                |> Cli.OptionsParser.with (Cli.Option.requiredKeywordArg "flags-from")
                |> Cli.OptionsParser.with (Cli.Option.requiredKeywordArg "output")
            )


toTask : Config -> BackendTask FatalError ()
toTask { flagsFrom, output } =
    buildDirectory flagsFrom
        |> BackendTask.andThen (writeFiles output)


buildDirectory : String -> BackendTask FatalError (Dict (List String) String)
buildDirectory path =
    Glob.succeed Tuple.pair
        |> Glob.match
            (Glob.literal
                (if String.endsWith "/" path then
                    path

                 else
                    path ++ "/"
                )
            )
        |> Glob.capture Glob.recursiveWildcard
        |> Glob.captureStats
        |> Glob.toBackendTask
        |> BackendTask.andThen
            (\fileList ->
                fileList
                    |> List.filterMap
                        (\( key, stats ) ->
                            if stats.isDirectory then
                                Nothing

                            else
                                File.rawFile stats.fullPath
                                    |> BackendTask.allowFatal
                                    |> BackendTask.map (Tuple.pair key)
                                    |> Just
                        )
                    |> BackendTask.combine
                    |> BackendTask.map Dict.fromList
            )


writeFiles : String -> Dict (List String) String -> BackendTask FatalError ()
writeFiles output input =
    case getLocaleData "en" input of
        Ok english ->
            generate english input
                |> ResultME.andThen
                    (\{ modulesStatus, languageFiles, allLocales } ->
                        let
                            shared : Shared
                            shared =
                                { english = english
                                , allLocales = allLocales
                                }

                            common : ResultME String (List Elm.File)
                            common =
                                commonFiles input shared modulesStatus
                        in
                        ResultME.map2 (++) common (Ok languageFiles)
                    )
                |> Result.mapError
                    (\errors ->
                        List.Nonempty.toList errors
                            |> String.join "\n"
                            |> FatalError.fromString
                    )
                |> BackendTask.fromResult
                |> BackendTask.andThen
                    (\files ->
                        files
                            |> List.map
                                (\file ->
                                    Pages.Script.writeFile
                                        { path = output ++ "/" ++ file.path
                                        , body = file.contents
                                        }
                                        |> BackendTask.allowFatal
                                )
                            |> BackendTask.sequence
                            |> BackendTask.map (\_ -> ())
                    )

        Err e ->
            e
                |> List.Nonempty.toList
                |> String.join "\n"
                |> FatalError.fromString
                |> BackendTask.fail


type alias Shared =
    { english : LocaleData
    , allLocales : Dict String LocaleData
    }


type alias LocaleData =
    { key : String
    , languages : Dict String String
    , languageNames : Set String
    , territories : Dict String String
    , scripts : Dict String String
    , variants : Dict String String
    }


type alias Locale =
    { key : String
    , fullEnglishName : String
    , fullNativeName : String
    , moduleName : ModuleName
    }


type alias Language =
    { fullEnglishName : String
    , fullNativeName : String
    , moduleName : ModuleName
    }


type alias ModuleName =
    List String


type alias ModuleStatus =
    { territories : DictStatus }


type DictStatus
    = Present
      -- Pointer to the parent ModuleName
    | Absent ModuleName


commonFiles : Dict (List String) String -> Shared -> Dict ModuleName ModuleStatus -> ResultME String (List Elm.File)
commonFiles files shared modulesStatus =
    let
        allLocales : ResultME String (List Locale)
        allLocales =
            files
                |> Dict.keys
                |> combineFilterMap
                    (\k ->
                        case k of
                            [ key, "territories.json" ] ->
                                case parseLanguageTag shared key of
                                    Ok { fullEnglishName, fullNativeName, moduleName } ->
                                        { key = key
                                        , fullEnglishName = fullEnglishName
                                        , fullNativeName = fullNativeName
                                        , moduleName = moduleName
                                        }
                                            |> Just
                                            |> Ok

                                    Err e ->
                                        Err e

                            _ ->
                                Ok Nothing
                    )

        defaultContent : Maybe (List String)
        defaultContent =
            Dict.get [ "defaultContent.json" ] files
                |> Maybe.andThen
                    (\json ->
                        let
                            defaultContentDecoder : Json.Decode.Decoder (List String)
                            defaultContentDecoder =
                                Json.Decode.at [ "defaultContent" ]
                                    (Json.Decode.list Json.Decode.string)
                        in
                        json
                            |> Json.Decode.decodeString defaultContentDecoder
                            |> Result.toMaybe
                    )

        likelySubtags : Maybe (Dict String String)
        likelySubtags =
            Dict.get [ "likelySubtags.json" ] files
                |> Maybe.andThen
                    (\json ->
                        let
                            likelySubtagsDecoder : Json.Decode.Decoder (Dict String String)
                            likelySubtagsDecoder =
                                Json.Decode.at [ "supplemental", "likelySubtags" ]
                                    (Json.Decode.dict Json.Decode.string)
                        in
                        json
                            |> Json.Decode.decodeString likelySubtagsDecoder
                            |> Result.toMaybe
                    )
    in
    allLocales
        |> ResultME.andThen
            (\all ->
                ResultME.combineList
                    [ localizedFile all modulesStatus
                    , mainFile all { defaultContent = defaultContent, likelySubtags = likelySubtags }
                    ]
            )


mainFile : List Locale -> { defaultContent : Maybe (List String), likelySubtags : Maybe (Dict String String) } -> ResultME String Elm.File
mainFile allLocales { defaultContent, likelySubtags } =
    ResultME.map
        (\likelySubtagsDeclaration ->
            Elm.file [ "Cldr" ]
                [ countryCodeTypeDeclaration
                , allLocalesDeclaration allLocales
                , localeToEnglishNameDeclaration allLocales
                , localeToNativeNameDeclaration allLocales
                , toAlpha2Declaration
                , fromAlpha2Declaration
                , allCountryCodesDeclaration
                , likelySubtagsDeclaration
                ]
        )
        (toLikelySubtagsDeclaration allLocales defaultContent likelySubtags)


toLikelySubtagsDeclaration :
    List Locale
    -> Maybe (List String)
    -> Maybe (Dict String String)
    -> ResultME String Elm.Declaration
toLikelySubtagsDeclaration allLocales defaultContentMaybe likelySubtagsMaybe =
    case ( likelySubtagsMaybe, defaultContentMaybe ) of
        ( Nothing, _ ) ->
            ResultME.error "Could not parse likelySubtags.json"

        ( _, Nothing ) ->
            ResultME.error "Could not parse defaultContent.json"

        ( Just likelySubtags, Just defaultContent ) ->
            (\locale ->
                Elm.Case.custom locale
                    Annotation.string
                    ((allLocales
                        |> List.filterMap
                            (\{ key } ->
                                let
                                    fromLikely : () -> Maybe Elm.Case.Branch
                                    fromLikely () =
                                        Dict.get key likelySubtags
                                            |> Maybe.map
                                                (\likelySubtag ->
                                                    Elm.Case.branch
                                                        (Elm.Arg.string key)
                                                        (\_ ->
                                                            Gen.Maybe.make_.just <|
                                                                Elm.string likelySubtag
                                                        )
                                                )
                                in
                                case
                                    List.filter
                                        (\line -> String.startsWith (key ++ "-") line)
                                        defaultContent
                                of
                                    [] ->
                                        fromLikely ()

                                    [ likelySubtag ] ->
                                        Elm.Case.branch
                                            (Elm.Arg.string key)
                                            (\_ -> Gen.Maybe.make_.just <| Elm.string likelySubtag)
                                            |> Just

                                    _ ->
                                        fromLikely ()
                            )
                     )
                        ++ [ Elm.Case.branch Elm.Arg.ignore (\_ -> Gen.Maybe.make_.nothing) ]
                    )
                    |> Elm.withType (Gen.Maybe.annotation_.maybe Annotation.string)
            )
                |> Elm.fn (Elm.Arg.varWith "locale" Annotation.string)
                |> Elm.declaration "likelySubtags"
                |> Elm.expose
                |> Ok


countryCodeTypeDeclaration : Elm.Declaration
countryCodeTypeDeclaration =
    allCountryCodes
        |> List.map Elm.variant
        |> Elm.customType "CountryCode"
        |> Elm.withDocumentation "All the supported country codes. `GT` and `LT` are defined in Basics so we define them as `GT_` and `LT_`."
        |> Elm.exposeConstructor


allCountryCodesDeclaration : Elm.Declaration
allCountryCodesDeclaration =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.named [] "CountryCode"
    in
    allCountryCodes
        |> List.map Elm.val
        |> Elm.list
        |> Elm.withType (Annotation.list countryCodeAnnotation)
        |> Elm.declaration "allCountryCodes"
        |> Elm.withDocumentation "All `CountryCode`s sorted alphabetically."
        |> Elm.expose


allLocalesDeclaration : List Locale -> Elm.Declaration
allLocalesDeclaration allLocales =
    allLocales
        |> List.map (\{ key } -> Elm.string key)
        |> Elm.list
        |> Elm.declaration "allLocales"
        |> Elm.withDocumentation "All the supported locales."
        |> Elm.expose


fromAlpha2Declaration : Elm.Declaration
fromAlpha2Declaration =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.named [] "CountryCode"

        implementation : Elm.Expression -> Elm.Expression
        implementation countryCodeExpr =
            Elm.Case.custom (Gen.String.call_.toLower countryCodeExpr)
                Annotation.string
                ((allCountryCodes
                    |> List.map
                        (\countryCode ->
                            Elm.Case.branch
                                (countryCode
                                    |> String.replace "_" ""
                                    |> String.toLower
                                    |> Elm.Arg.string
                                )
                                (\_ ->
                                    Elm.val countryCode
                                        |> Gen.Maybe.make_.just
                                )
                        )
                 )
                    ++ [ Elm.Case.branch
                            Elm.Arg.ignore
                            (\_ -> Gen.Maybe.make_.nothing)
                       ]
                )
                |> Elm.withType (Annotation.maybe countryCodeAnnotation)
    in
    implementation
        |> Elm.fn (Elm.Arg.varWith "countryCode" Annotation.string)
        |> Elm.declaration "fromAlpha2"
        |> Elm.withDocumentation "`CountryCode` from two-letter `ISO 3166-1 alpha-2`."
        |> Elm.expose


toAlpha2Declaration : Elm.Declaration
toAlpha2Declaration =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.named [] "CountryCode"

        implementation : Elm.Expression -> Elm.Expression
        implementation countryCodeExpr =
            allCountryCodes
                |> List.map
                    (\countryCode ->
                        Elm.Case.branch
                            (Elm.Arg.customType countryCode ())
                            (\_ ->
                                countryCode
                                    |> String.toLower
                                    |> String.replace "_" ""
                                    |> Elm.string
                            )
                    )
                |> Elm.Case.custom countryCodeExpr countryCodeAnnotation
    in
    implementation
        |> Elm.fn (Elm.Arg.varWith "countryCode" countryCodeAnnotation)
        |> Elm.declaration "toAlpha2"
        |> Elm.withDocumentation "Two-letter `ISO 3166-1 alpha-2` code from `CountryCode`."
        |> Elm.expose


localeToEnglishNameDeclaration : List Locale -> Elm.Declaration
localeToEnglishNameDeclaration allLocales =
    let
        implementation : Elm.Expression -> Elm.Expression
        implementation =
            caseOnLocale allLocales
                { case_ =
                    \{ fullEnglishName } ->
                        Elm.string fullEnglishName
                            |> Gen.Maybe.make_.just
                            |> Just
                            |> Ok
                , otherwise = Gen.Maybe.make_.nothing
                }
                |> Result.mapError (\es -> List.Nonempty.head es |> never)
                |> Result.Extra.merge
    in
    implementation
        |> Elm.fn (Elm.Arg.varWith "locale" Annotation.string)
        |> Elm.withType
            (Annotation.function
                [ Annotation.string ]
                (Annotation.maybe Annotation.string)
            )
        |> Elm.declaration "localeToEnglishName"
        |> Elm.withDocumentation "Get the english name of a locale."
        |> Elm.expose


localeToNativeNameDeclaration : List Locale -> Elm.Declaration
localeToNativeNameDeclaration allLocales =
    let
        implementation : Elm.Expression -> Elm.Expression
        implementation =
            caseOnLocale allLocales
                { case_ =
                    \{ fullNativeName } ->
                        if String.isEmpty fullNativeName then
                            Ok Nothing

                        else
                            Ok (Just <| Gen.Maybe.make_.just <| Elm.string fullNativeName)
                , otherwise = Gen.Maybe.make_.nothing
                }
                |> Result.mapError (\es -> List.Nonempty.head es |> never)
                |> Result.Extra.merge
    in
    implementation
        |> Elm.fn (Elm.Arg.varWith "locale" Annotation.string)
        |> Elm.withType
            (Annotation.function
                [ Annotation.string ]
                (Annotation.maybe Annotation.string)
            )
        |> Elm.declaration "localeToNativeName"
        |> Elm.withDocumentation "Get the english name of a locale."
        |> Elm.expose


{-| Split the input on dashes and pattern match with the longest prefix we know of.
-}
caseOnLocale :
    List Locale
    ->
        { case_ : Locale -> ResultME e (Maybe Elm.Expression)
        , otherwise : Elm.Expression
        }
    -> ResultME e (Elm.Expression -> Elm.Expression)
caseOnLocale allLocales { case_, otherwise } =
    allLocales
        |> List.map
            (\locale ->
                let
                    splat : List String
                    splat =
                        String.split "-" locale.key
                in
                ( splat, locale )
            )
        |> List.sortWith
            (\( l, _ ) ( r, _ ) -> sortSplitLocale l r)
        |> combineFilterMap
            (\( splat, locale ) ->
                case case_ locale of
                    Ok Nothing ->
                        Ok Nothing

                    Ok (Just expr) ->
                        Elm.Case.branch
                            (Elm.Arg.list (\_ _ -> expr)
                                |> Elm.Arg.items (List.map Elm.Arg.string splat)
                                |> Elm.Arg.listRemaining "_"
                            )
                            identity
                            |> Just
                            |> Ok

                    Err e ->
                        Err e
            )
        |> Result.map
            (\cases input ->
                Elm.Case.custom
                    (Gen.String.call_.split (Elm.string "-") input)
                    (Annotation.list Annotation.string)
                    (cases ++ [ Elm.Case.branch Elm.Arg.ignore (\_ -> otherwise) ])
            )


{-| Split the input on dashes and pattern match with the longest prefix we know of.
-}
caseOnLocaleWith :
    List Locale
    ->
        { case_ : Locale -> ResultME e (Maybe (a -> Elm.Expression))
        , otherwise : Elm.Expression
        }
    -> ResultME e (a -> Elm.Expression -> Elm.Expression)
caseOnLocaleWith allLocales { case_, otherwise } =
    allLocales
        |> List.map
            (\locale ->
                let
                    splat : List String
                    splat =
                        String.split "-" locale.key
                in
                ( splat, locale )
            )
        |> List.sortWith
            (\( l, _ ) ( r, _ ) -> sortSplitLocale l r)
        |> combineFilterMap
            (\( splat, locale ) ->
                case case_ locale of
                    Ok Nothing ->
                        Ok Nothing

                    Ok (Just toExpr) ->
                        (\v ->
                            Elm.Case.branch
                                (Elm.Arg.list (\_ _ -> toExpr v)
                                    |> Elm.Arg.items (List.map Elm.Arg.string splat)
                                    |> Elm.Arg.listRemaining "_"
                                )
                                identity
                        )
                            |> Just
                            |> Ok

                    Err e ->
                        Err e
            )
        |> ResultME.map
            (\cases v input ->
                Elm.Case.custom
                    (Gen.String.call_.split (Elm.string "-") input)
                    (Annotation.list Annotation.string)
                    (List.map (\toCase -> toCase v) cases ++ [ Elm.Case.branch Elm.Arg.ignore (\_ -> otherwise) ])
            )


sortSplitLocale : List comparable -> List comparable -> Order
sortSplitLocale l r =
    -- We want to sort alphabetically, but have longer lists first so we can match from most specific to least specific
    case ( l, r ) of
        ( [], [] ) ->
            EQ

        ( [], _ :: _ ) ->
            GT

        ( _ :: _, [] ) ->
            LT

        ( lh :: lt, rh :: rt ) ->
            let
                cmp : Order
                cmp =
                    compare lh rh
            in
            if cmp == EQ then
                sortSplitLocale lt rt

            else
                cmp


localizedFile : List Locale -> Dict ModuleName ModuleStatus -> ResultME String Elm.File
localizedFile allLocales modulesStatus =
    ResultME.map
        (\declaration ->
            Elm.file [ "Cldr", "Localized" ]
                [ declaration
                ]
        )
        (localizedCountryCodeToNameDeclaration allLocales modulesStatus)


localizedCountryCodeToNameDeclaration : List Locale -> Dict ModuleName ModuleStatus -> ResultME String Elm.Declaration
localizedCountryCodeToNameDeclaration allLocales modulesStatus =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.named [ "Cldr" ] "CountryCode"

        functionAnnotation : Annotation
        functionAnnotation =
            Annotation.function
                [ countryCodeAnnotation ]
                Annotation.string

        implementation : ResultME String (Elm.Expression -> Elm.Expression -> Elm.Expression)
        implementation =
            caseOnLocaleWith allLocales
                { case_ = case_
                , otherwise = Gen.Maybe.make_.nothing
                }

        case_ : Locale -> ResultME String (Maybe (Elm.Expression -> Elm.Expression))
        case_ { moduleName } =
            let
                go : Bool -> ModuleName -> ResultME String (Maybe (Elm.Expression -> Elm.Expression))
                go first name =
                    case Dict.get name modulesStatus of
                        Just { territories } ->
                            case territories of
                                Present ->
                                    (\countryCode ->
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = "Cldr" :: name
                                                , name = "countryCodeToName"
                                                , annotation = Just functionAnnotation
                                                }
                                            )
                                            [ countryCode ]
                                            |> Gen.Maybe.make_.just
                                    )
                                        |> Just
                                        |> Ok

                                Absent parent ->
                                    if first && List.Extra.isPrefixOf parent name then
                                        Ok Nothing

                                    else
                                        go False parent

                        Nothing ->
                            ResultME.error ("Could not find data for " ++ String.join "." name)
            in
            go True moduleName
    in
    implementation
        |> Result.map
            (\n ->
                n
                    |> Elm.fn2
                        (Elm.Arg.varWith "locale" Annotation.string)
                        (Elm.Arg.varWith "countryCode" countryCodeAnnotation)
                    |> Elm.withType
                        (Annotation.function
                            [ Annotation.string, countryCodeAnnotation ]
                            (Annotation.maybe Annotation.string)
                        )
                    |> Elm.declaration "countryCodeToName"
                    |> Elm.expose
            )


generate :
    LocaleData
    -> Dict (List String) String
    ->
        ResultME
            String
            { modulesStatus : Dict ModuleName ModuleStatus
            , languageFiles : List Elm.File
            , allLocales : Dict String LocaleData
            }
generate english files =
    let
        tryAddDictionary :
            String
            -> ResultME String (Maybe ( ModuleName, { fullEnglishName : String, data : LocaleData } ))
        tryAddDictionary key =
            case parseLanguageTag { english = english, allLocales = Dict.empty } key of
                Ok { fullEnglishName, moduleName } ->
                    getLocaleData key files
                        |> ResultME.mapError (\e -> "Error for " ++ fullEnglishName ++ ": " ++ e)
                        |> Result.map
                            (\data ->
                                ( moduleName
                                , { fullEnglishName = fullEnglishName
                                  , data = data
                                  }
                                )
                                    |> Just
                            )

                Err err ->
                    if key == "und" then
                        -- Unknown language
                        Ok Nothing

                    else
                        let
                            name : String
                            name =
                                Dict.get key english.languages
                                    |> Maybe.withDefault ("key - " ++ key)

                            msg : String
                            msg =
                                "Failed to parse language tag, language name is " ++ name ++ ", error is: " ++ String.join ", " (List.Nonempty.toList err)
                        in
                        ResultME.error ("Error for " ++ key ++ ": " ++ msg)
    in
    files
        |> Dict.keys
        |> combineFilterMap
            (\k ->
                case k of
                    [ key, "territories.json" ] ->
                        tryAddDictionary key

                    _ ->
                        Ok Nothing
            )
        |> ResultME.map Dict.fromList
        |> ResultME.andThen
            (\allDictionaries ->
                allDictionaries
                    |> Dict.toList
                    |> List.sortBy
                        (\( moduleName, { data } ) ->
                            ( List.length moduleName
                            , List.length (getParentModule data.territories moduleName)
                            )
                        )
                    |> Result.Extra.foldlWhileOk
                        (\( moduleName, { fullEnglishName, data } ) acc ->
                            let
                                parentModuleName : ModuleName
                                parentModuleName =
                                    getParentModule data.territories moduleName

                                parent :
                                    Maybe
                                        { fullEnglishName : String
                                        , data : LocaleData
                                        }
                                parent =
                                    Dict.get parentModuleName allDictionaries
                            in
                            case
                                countryCodeToNameDeclaration
                                    { parentModuleName = parentModuleName }
                                    parent
                                    { fullEnglishName = fullEnglishName
                                    , territories = data.territories
                                    }
                                    acc.modulesStatus
                            of
                                Ok (Just declaration) ->
                                    { acc
                                        | languageFiles =
                                            Elm.file ("Cldr" :: moduleName)
                                                [ declaration
                                                ]
                                                :: acc.languageFiles
                                        , modulesStatus =
                                            Dict.insert moduleName
                                                { territories = Present }
                                                acc.modulesStatus
                                        , allLocales = Dict.insert data.key data acc.allLocales
                                    }
                                        |> Ok

                                Ok Nothing ->
                                    { acc
                                        | modulesStatus =
                                            Dict.insert moduleName
                                                { territories = Absent parentModuleName }
                                                acc.modulesStatus
                                        , allLocales = Dict.insert data.key data acc.allLocales
                                    }
                                        |> Ok

                                Err e ->
                                    ResultME.error e
                        )
                        { languageFiles = [], modulesStatus = Dict.empty, allLocales = Dict.empty }
            )


getParentModule : Dict String String -> ModuleName -> ModuleName
getParentModule territories moduleName =
    case moduleName of
        [ "Spanish", region ] ->
            if region == "ElSalvador" then
                [ "Spanish", "PuertoRico" ]

            else if List.member region likeBrazil then
                [ "Spanish", "Brazil" ]

            else if List.member region likeArgentina then
                [ "Spanish", "Argentina" ]

            else
                [ "Spanish" ]

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


likeBrazil : List String
likeBrazil =
    [ "Belize"
    , "Cuba"
    , "LatinAmerica"
    , "Uruguay"
    ]


likeArgentina : List String
likeArgentina =
    [ "Bolivia"
    , "Brazil"
    , "Chile"
    , "Colombia"
    , "CostaRica"
    , "DominicanRepublic"
    , "Ecuador"
    , "ElSalvador"
    , "Guatemala"
    , "Honduras"
    , "Mexico"
    , "Nicaragua"
    , "Panama"
    , "Paraguay"
    , "Peru"
    , "PuertoRico"
    , "UnitedStates"
    , "Venezuela"
    ]


parseLanguageTag :
    Shared
    -> String
    -> ResultME String Language
parseLanguageTag { english, allLocales } key =
    if key == "und" then
        ResultME.error "Undefined language"

    else
        let
            traverse : (a -> ResultME String b) -> Maybe a -> ResultME String (Maybe b)
            traverse f v =
                case v of
                    Nothing ->
                        Ok Nothing

                    Just w ->
                        ResultME.map Just (f w)

            getData :
                Language.Language
                -> LanguageTag.Options
                -> LocaleData
                ->
                    ResultME
                        String
                        { languageName : String
                        , splitLanguageName : List String
                        , scriptName : Maybe String
                        , regionName : Maybe String
                        , variantName : Maybe String
                        }
            getData language options localeData =
                ResultME.map4
                    (\( languageName, splitLanguageName ) scriptName regionName variantName ->
                        { languageName = languageName
                        , splitLanguageName = splitLanguageName
                        , scriptName = scriptName
                        , regionName = regionName
                        , variantName = variantName
                        }
                    )
                    (languageToString localeData language)
                    (traverse (scriptToString localeData) options.script)
                    (traverse (regionToString localeData) options.region)
                    (variantsToString localeData options.variants)

            languageToString : LocaleData -> Language.Language -> ResultME String ( String, List String )
            languageToString localeData language =
                let
                    languageCode : String
                    languageCode =
                        Language.toCodeString language
                in
                case Dict.get languageCode localeData.languages of
                    Nothing ->
                        ResultME.error <| "Language not found: " ++ languageCode

                    Just languageName ->
                        case splitLanguage localeData languageName of
                            Nothing ->
                                ResultME.error <| "Failed to split language name: " ++ languageName

                            Just splat ->
                                Ok ( languageName, splat )
        in
        case LanguageTag.Parser.parseBcp47 key of
            Just ( language, options ) ->
                if not (List.isEmpty options.extensions) then
                    ResultME.error <| "Unsupported! extension = " ++ String.join ", " (List.map ExtendedLanguage.toCodeString options.extensions)

                else
                    case options.privateUse of
                        Just privateUseParts ->
                            ResultME.error <| "Unsupported! privateUse = " ++ PrivateUse.toCodeString privateUseParts

                        Nothing ->
                            ResultME.map2
                                (\englishData nativeData ->
                                    { fullEnglishName = fullLanguageName englishData
                                    , fullNativeName =
                                        Maybe.map fullLanguageName nativeData
                                            |> Maybe.withDefault ""
                                    , moduleName = toModuleName englishData
                                    }
                                )
                                (getData language options english)
                                (case Dict.get key allLocales of
                                    Nothing ->
                                        Ok Nothing

                                    Just localeData ->
                                        getData language options localeData
                                            |> Result.toMaybe
                                            |> Ok
                                )

            Nothing ->
                ResultME.error <| "Could not parse BCP 47 tag: " ++ key


fullLanguageName :
    { a
        | languageName : String
        , scriptName : Maybe String
        , regionName : Maybe String
        , variantName : Maybe String
    }
    -> String
fullLanguageName { languageName, scriptName, regionName, variantName } =
    let
        wrapString : String -> Maybe String -> String -> String
        wrapString before value after =
            case value of
                Nothing ->
                    ""

                Just w ->
                    before ++ w ++ after
    in
    languageName
        ++ wrapString " (" scriptName ")"
        ++ wrapString " - " regionName ""
        ++ wrapString " (" variantName ")"


toModuleName :
    { a
        | splitLanguageName : List String
        , scriptName : Maybe String
        , regionName : Maybe String
        , variantName : Maybe String
    }
    -> ModuleName
toModuleName { splitLanguageName, scriptName, regionName, variantName } =
    (splitLanguageName
        ++ List.filterMap identity
            [ scriptName
            , regionName
            , variantName
            ]
    )
        |> List.map
            (\name ->
                name
                    |> cleanName
                    |> String.replace "orthography" ""
                    |> String.replace " " ""
            )


variantsToString : LocaleData -> List Variant -> ResultME String (Maybe String)
variantsToString localeData variants =
    case variants of
        [ variant ] ->
            ResultME.map Just <| variantToString localeData variant

        [] ->
            Ok Nothing

        _ ->
            ResultME.error "Multiple variant are not supported"


variantToString : LocaleData -> Variant -> ResultME String String
variantToString localeData variant =
    let
        variantCode : String
        variantCode =
            Variant.toCodeString variant
    in
    case Dict.get variantCode localeData.variants of
        Nothing ->
            ResultME.error <|
                "Could not find variant: "
                    ++ variantCode
                    ++ " in locale "
                    ++ localeData.key
                    ++ ", available variants are [ "
                    ++ String.join ", " (Dict.keys localeData.variants)
                    ++ " ]"

        Just variantName ->
            Ok variantName


regionToString : LocaleData -> Region -> ResultME String String
regionToString localeData region =
    let
        regionCode : String
        regionCode =
            Region.toCodeString region
    in
    case Dict.get regionCode localeData.territories of
        Nothing ->
            ResultME.error <| "Could not find region: " ++ regionCode

        Just territoryName ->
            Ok territoryName


scriptToString : LocaleData -> Script -> ResultME String String
scriptToString localeData script =
    let
        scriptString : String
        scriptString =
            Script.toCodeString script
    in
    case Dict.get scriptString localeData.scripts of
        Nothing ->
            ResultME.error <| "Could not find script: " ++ scriptString

        Just name ->
            Ok name


getLocaleData : String -> Dict (List String) String -> ResultME String LocaleData
getLocaleData key files =
    ResultME.map4
        (\languages territories scripts variants ->
            { key = key
            , languages = languages
            , languageNames =
                if key == "en" then
                    languages
                        |> Dict.values
                        |> Set.fromList
                        |> Set.insert "Pidgin"
                        |> Set.insert "Gaelic"

                else
                    languages
                        |> Dict.values
                        |> Set.fromList
            , territories = territories
            , scripts = scripts
            , variants = variants
            }
        )
        (getFile key "languages" files)
        (getTerritories key files)
        (getFile key "scripts" files)
        (getVariants key files)


getTerritories : String -> Dict (List String) String -> ResultME String (Dict String String)
getTerritories key files =
    let
        fixup : Dict String String -> Dict String String
        fixup dict =
            dict
                |> replaceWithVariant "CD"
                |> replaceWithVariant "CG"
                |> replaceWithVariant "CZ"
                |> replaceWithShort "HK"
                |> replaceWithShort "MO"
                |> replaceWithShort "PS"
                |> replaceWithVariant "TL"

        replaceWithVariant : String -> Dict String String -> Dict String String
        replaceWithVariant k =
            replace k (k ++ "-alt-variant")

        replaceWithShort : String -> Dict String String -> Dict String String
        replaceWithShort k =
            replace k (k ++ "-alt-short")

        replace : String -> String -> Dict String String -> Dict String String
        replace to from dict =
            case Dict.get from dict of
                Nothing ->
                    dict

                Just v ->
                    Dict.insert to v dict
    in
    getFile key "territories" files
        |> ResultME.map fixup


getVariants : String -> Dict (List String) String -> ResultME String (Dict String String)
getVariants key files =
    let
        fixup : Dict String v -> Dict String v
        fixup dict =
            Dict.foldl
                (\k v acc ->
                    Dict.insert (String.toLower k) v acc
                )
                Dict.empty
                dict
    in
    getFile key "variants" files
        |> Result.map fixup
        -- Some locales don't have a variants.json file
        |> Result.withDefault Dict.empty
        |> Ok


getFile : String -> String -> Dict (List String) String -> ResultME String (Dict String String)
getFile key fileName files =
    let
        fullPath : List String
        fullPath =
            [ key, fileName ++ ".json" ]
    in
    case Dict.get fullPath files of
        Just json ->
            Json.Decode.decodeString
                (Json.Decode.at
                    [ "main"
                    , key
                    , "localeDisplayNames"
                    , fileName
                    ]
                    (Json.Decode.dict Json.Decode.string)
                )
                json
                |> Result.mapError
                    (\e ->
                        "\"" ++ String.join "/" fullPath ++ "\": decoding failed: " ++ Json.Decode.errorToString e
                    )
                |> ResultME.fromResult

        Nothing ->
            ResultME.error <| "Could not find \"" ++ String.join "/" fullPath ++ "\""


allCountryCodes : List String
allCountryCodes =
    Iso3166.all
        |> List.map (Iso3166.toAlpha2 >> toVariantName)
        -- Kosovo
        |> (::) "XK"
        |> List.sort


toVariantName : String -> String
toVariantName input =
    input
        |> String.toUpper
        |> String.replace "GT" "GT_"
        |> String.replace "LT" "LT_"


{-| Returns nothing if it's identical to the parent language.
-}
countryCodeToNameDeclaration :
    { parentModuleName : ModuleName }
    ->
        Maybe
            { fullEnglishName : String
            , data : LocaleData
            }
    ->
        { fullEnglishName : String
        , territories : Dict String String
        }
    -> Dict ModuleName ModuleStatus
    -> Result String (Maybe Elm.Declaration)
countryCodeToNameDeclaration { parentModuleName } parent { fullEnglishName, territories } modulesStatus =
    let
        countryCodeAnnotation : Annotation
        countryCodeAnnotation =
            Annotation.namedWith [ "Cldr" ] "CountryCode" []

        parentFunction : Result String Elm.Expression
        parentFunction =
            let
                go : List String -> Result String Elm.Expression
                go name =
                    case Dict.get name modulesStatus of
                        Nothing ->
                            Err ("Could not find module info for " ++ String.join "." name)

                        Just status ->
                            case status.territories of
                                Present ->
                                    Elm.value
                                        { importFrom = "Cldr" :: name
                                        , name = "countryCodeToName"
                                        , annotation = Just <| Annotation.function [ countryCodeAnnotation ] Annotation.string
                                        }
                                        |> Ok

                                Absent absent ->
                                    go absent
            in
            go parentModuleName

        parentTerritories : Maybe (Dict String String)
        parentTerritories =
            parent
                |> Maybe.map (\{ data } -> data.territories)

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
                                    let
                                        parentName : Maybe String
                                        parentName =
                                            parentTerritories
                                                |> Maybe.andThen (Dict.get countryCodeClean)
                                    in
                                    if Just name == parentName then
                                        Nothing

                                    else
                                        Just <|
                                            Elm.Case.branch
                                                (Elm.Arg.customType countryCode ())
                                                (\_ -> Elm.string name)
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
        -- Nothing
        parentFunction
            |> Result.map
                (\f ->
                    f
                        |> Elm.declaration "countryCodeToName"
                        |> Elm.withDocumentation
                            ("Name for `CountryCode` in "
                                ++ fullEnglishName
                                ++ (case parent of
                                        Nothing ->
                                            ".\n\n"

                                        Just pt ->
                                            ".\n\nThis is identical to the " ++ pt.fullEnglishName ++ " version.\n\n"
                                   )
                                ++ table
                            )
                        |> Elm.expose
                        |> Just
                )

    else
        parentFunction
            |> Result.map
                (\f ->
                    Elm.fn (Elm.Arg.varWith "countryCode" countryCodeAnnotation)
                        (\countryCodeExpr ->
                            (if List.length branches == List.length allCountryCodes then
                                branches

                             else
                                branches
                                    ++ [ Elm.Case.branch Elm.Arg.ignore
                                            (\_ -> Elm.apply f [ countryCodeExpr ])
                                       ]
                            )
                                |> Elm.Case.custom countryCodeExpr countryCodeAnnotation
                        )
                        |> Elm.declaration "countryCodeToName"
                        |> Elm.withDocumentation ("Name for `CountryCode` in " ++ fullEnglishName ++ ".\n\n" ++ table)
                        |> Elm.expose
                        |> Just
                )


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


splitLanguage : LocaleData -> String -> Maybe (List String)
splitLanguage localeData lang =
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
            if Set.member suffix localeData.languageNames then
                Just [ suffix, prefix ]

            else if Set.member prefix localeData.languageNames then
                Just [ prefix, suffix ]

            else
                Nothing

        head :: tail ->
            if Set.member head localeData.languageNames then
                Just [ head, String.concat <| List.reverse tail ]

            else
                Nothing

        _ ->
            Nothing


combineFilterMap :
    (a -> ResultME.ResultME err (Maybe b))
    -> List a
    -> ResultME err (List b)
combineFilterMap f xs =
    xs
        |> ResultME.combineMap f
        |> ResultME.map Maybe.Extra.values
