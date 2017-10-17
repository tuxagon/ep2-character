module Main exposing (..)

import Dict exposing (Dict)
import EclipsePhase2.Styles as Styles exposing (Styles)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Html exposing (Html)


type SkillCategory
    = Combat
    | Mental
    | Physical
    | Psi
    | Social
    | Technical
    | Vehicle


type SkillKind
    = Know
    | Active SkillCategory


type Aptitude
    = Cognition
    | Intuition
    | Reflexes
    | Savvy
    | Somatics
    | Willpower


type alias Field =
    String


type SkillName
    = Athletics
    | Deceive
    | Exotic Field
    | Fray
    | FreeFall
    | Guns
    | Hardware Field
    | Infiltrate
    | Infosec
    | Interface
    | Kinesics
    | Know_ Field
    | Medicine Field
    | Melee
    | Perceive
    | Persuade
    | Pilot Field
    | Program
    | Provoke
    | Psi_
    | Research
    | Survival


type alias SkillInformation =
    { aptitude : Aptitude
    , baseStat : Int
    , kind : SkillKind
    , specializations : List String
    }


type Skill
    = Skill SkillName SkillInformation


type alias Model =
    { skills : List Skill
    , skillField : Maybe String
    , aptitudeMenu : Input.SelectWith Aptitude Msg
    , count : Int
    , skillSearch : Input.SelectWith SkillName Msg
    }


init : ( Model, Cmd Msg )
init =
    ( { skills = []
      , skillField = Nothing
      , aptitudeMenu = Input.dropMenu Nothing ChooseAptitude
      , count = 0
      , skillSearch = Input.autocomplete Nothing Search
      }
    , Cmd.none
    )


buildSkill : String -> Aptitude -> Maybe Skill
buildSkill skill aptitude =
    case skillName skill of
        Nothing ->
            Nothing

        Just name ->
            Just <| Skill name <| initSkillInformation aptitude


initSkillInformation : Aptitude -> SkillInformation
initSkillInformation aptitude =
    { aptitude = aptitude
    , baseStat = 0
    , kind = Know
    , specializations = []
    }


defaultSkillName : SkillName
defaultSkillName =
    Athletics


defaultAptitude : Aptitude
defaultAptitude =
    Cognition


type Msg
    = NoOp
    | EditSkill String
    | AddSkill
    | Search (Input.SelectMsg SkillName)
    | ChooseAptitude (Input.SelectMsg Aptitude)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EditSkill skill ->
            ( { model
                | skillField =
                    if String.trim skill == "" then
                        Nothing
                    else
                        Just skill
              }
            , Cmd.none
            )

        AddSkill ->
            let
                field =
                    Maybe.withDefault "" model.skillField

                good =
                    not (requiresField selectedSkillName)
                        || (requiresField selectedSkillName && hasField field)

                selectedSkillName : SkillName
                selectedSkillName =
                    Maybe.withDefault defaultSkillName <|
                        Input.selected model.skillSearch

                selectedAptitude : Aptitude
                selectedAptitude =
                    case selectedSkillName of
                        Exotic _ ->
                            Maybe.withDefault
                                (skillAptitude selectedSkillName defaultAptitude)
                            <|
                                Input.selected model.aptitudeMenu

                        name ->
                            skillAptitude name defaultAptitude

                config =
                    { aptitude = selectedAptitude
                    , field = field
                    }

                updatedSkills =
                    model.skills
                        |> (++) [ (newSkill selectedSkillName config) ]
                        |> List.sortBy (\(Skill name _) -> skillNameText name)
            in
                if good then
                    ( { model
                        | skills = updatedSkills
                        , skillField = Nothing
                        , count = model.count + 1
                      }
                    , Cmd.none
                    )
                else
                    ( model, Cmd.none )

        Search searchMsg ->
            let
                updatedSearch =
                    Input.updateSelection searchMsg model.skillSearch
            in
                ( { model
                    | skillSearch = updatedSearch
                  }
                , Cmd.none
                )

        ChooseAptitude chooseMsg ->
            ( { model
                | aptitudeMenu = Input.updateSelection chooseMsg model.aptitudeMenu
              }
            , Cmd.none
            )


hasField : Field -> Bool
hasField field =
    field
        |> String.trim
        |> String.isEmpty
        |> not


requiresField : SkillName -> Bool
requiresField name =
    case name of
        Exotic _ ->
            True

        Hardware _ ->
            True

        Know_ _ ->
            True

        Medicine _ ->
            True

        Pilot _ ->
            True

        _ ->
            False


newSkill : SkillName -> { aptitude : Aptitude, field : Field } -> Skill
newSkill name config =
    let
        skillInfo =
            initSkillInformation config.aptitude

        noField =
            Skill name skillInfo

        withField =
            Skill (joinSkillName ( name, config.field )) skillInfo
    in
        case name of
            Exotic _ ->
                withField

            Hardware _ ->
                withField

            Know_ _ ->
                withField

            Medicine _ ->
                withField

            Pilot _ ->
                withField

            _ ->
                noField


view : Model -> Html Msg
view model =
    Element.layout Styles.stylesheet <|
        column Styles.None
            []
            [ viewSkills model.skills
            , viewSkillForm model
            ]


viewSkills : List Skill -> Element Styles variation Msg
viewSkills skills =
    column Styles.None
        []
        (List.map viewSkill skills)


viewSkill : Skill -> Element Styles variation Msg
viewSkill (Skill name info) =
    let
        skillText =
            skillNameText name

        aptitudeText =
            toCode info.aptitude
    in
        el Styles.None
            []
            (text <| skillText ++ " (" ++ aptitudeText ++ ")")


viewSkillForm : Model -> Element Styles variation Msg
viewSkillForm model =
    let
        viewChoice ( key, val ) =
            Input.choice val (text key)
    in
        row Styles.None
            [ width (percent 50) ]
            (List.concat
                [ [ Input.select Styles.Border
                        []
                        { label = Input.hiddenLabel "Skill"
                        , with = model.skillSearch
                        , options = []
                        , max = List.length skillsMap
                        , menu =
                            Input.menu Styles.None
                                []
                                (List.map viewChoice skillsMap)
                        }
                  ]
                , viewSkillFormInput model <|
                    Maybe.withDefault Athletics
                        (Input.selected model.skillSearch)
                , [ button Styles.None
                        [ onClick AddSkill
                        , alignLeft
                        ]
                        (text "Add Skill")
                  ]
                ]
            )


viewSkillFormInput : Model -> SkillName -> List (Element Styles variation Msg)
viewSkillFormInput model name =
    let
        aptitudeChoice aptitude =
            Input.choice aptitude (text <| toCode aptitude)

        noField =
            [ empty ]

        withField =
            [ Input.text Styles.Border
                []
                { onChange = EditSkill
                , value = Maybe.withDefault "" model.skillField
                , label = Input.hiddenLabel "Field"
                , options =
                    [ Input.textKey (toString model.count)
                    ]
                }
            ]

        withFieldAndAptitudes =
            withField
                ++ [ (Input.select Styles.Border
                        []
                        { label = Input.hiddenLabel "Aptitude"
                        , with = model.aptitudeMenu
                        , options = []
                        , max = 6
                        , menu =
                            Input.menu Styles.Border
                                []
                                [ aptitudeChoice Cognition
                                , aptitudeChoice Intuition
                                , aptitudeChoice Reflexes
                                , aptitudeChoice Savvy
                                , aptitudeChoice Somatics
                                , aptitudeChoice Willpower
                                ]
                        }
                     )
                   ]
    in
        case name of
            Exotic _ ->
                withFieldAndAptitudes

            Hardware _ ->
                withField

            Know_ _ ->
                withField

            Medicine _ ->
                withField

            Pilot _ ->
                withField

            _ ->
                noField


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


joinSkillName : ( SkillName, Field ) -> SkillName
joinSkillName ( name, field ) =
    case name of
        Exotic _ ->
            Exotic field

        Hardware _ ->
            Hardware field

        Know_ _ ->
            Know_ field

        Medicine _ ->
            Medicine field

        Pilot _ ->
            Pilot field

        _ ->
            name


skillAptitude : SkillName -> Aptitude -> Aptitude
skillAptitude name default =
    case name of
        Athletics ->
            Somatics

        Deceive ->
            Savvy

        Exotic _ ->
            default

        Fray ->
            Reflexes

        FreeFall ->
            Somatics

        Guns ->
            Reflexes

        Hardware _ ->
            Cognition

        Infiltrate ->
            Reflexes

        Infosec ->
            Cognition

        Interface ->
            Cognition

        Kinesics ->
            Savvy

        Know_ _ ->
            Cognition

        Medicine _ ->
            Cognition

        Melee ->
            Somatics

        Perceive ->
            Intuition

        Persuade ->
            Savvy

        Pilot _ ->
            Reflexes

        Program ->
            Cognition

        Provoke ->
            Savvy

        Psi_ ->
            Willpower

        Research ->
            Intuition

        Survival ->
            Intuition


skillNameText : SkillName -> String
skillNameText name =
    case name of
        Athletics ->
            "Athletics"

        Deceive ->
            "Deceive"

        Exotic exoticField ->
            "Exotic: " ++ exoticField

        Fray ->
            "Fray"

        FreeFall ->
            "Free Fall"

        Guns ->
            "Guns"

        Hardware hardwareField ->
            "Hardware: " ++ hardwareField

        Infiltrate ->
            "Infiltrate"

        Infosec ->
            "Infosec"

        Interface ->
            "Interface"

        Kinesics ->
            "Kinesics"

        Know_ knowField ->
            "Know: " ++ knowField

        Medicine medicineField ->
            "Medicine: " ++ medicineField

        Melee ->
            "Melee"

        Perceive ->
            "Perceive"

        Persuade ->
            "Persuade"

        Pilot pilotField ->
            "Pilot: " ++ pilotField

        Program ->
            "Program"

        Provoke ->
            "Provoke"

        Psi_ ->
            "Psi"

        Research ->
            "Research"

        Survival ->
            "Survival"


skillName : String -> Maybe SkillName
skillName key =
    skillsMap
        |> List.map (\( k, v ) -> ( String.toLower k, v ))
        |> Dict.fromList
        |> Dict.get (String.toLower key)


skillsMap : List ( String, SkillName )
skillsMap =
    [ ( "Athletics", Athletics )
    , ( "Deceive", Deceive )
    , ( "Exotic", Exotic "" )
    , ( "Fray", Fray )
    , ( "Free Fall", FreeFall )
    , ( "Guns", Guns )
    , ( "Hardware", Hardware "" )
    , ( "Infiltrate", Infiltrate )
    , ( "Infosec", Infosec )
    , ( "Interface", Interface )
    , ( "Kinesics", Kinesics )
    , ( "Know", Know_ "" )
    , ( "Medicine", Medicine "" )
    , ( "Melee", Melee )
    , ( "Perceive", Perceive )
    , ( "Persuade", Persuade )
    , ( "Pilot", Pilot "" )
    , ( "Program", Program )
    , ( "Provoke", Provoke )
    , ( "Psi", Psi_ )
    , ( "Research", Research )
    , ( "Survival", Survival )
    ]


toCode : Aptitude -> String
toCode aptitude =
    case aptitude of
        Cognition ->
            "COG"

        Intuition ->
            "INT"

        Reflexes ->
            "REF"

        Savvy ->
            "SAV"

        Somatics ->
            "SOM"

        Willpower ->
            "WIL"


fromCode : String -> Maybe Aptitude
fromCode code =
    case code of
        "COG" ->
            Just Cognition

        "INT" ->
            Just Intuition

        "REF" ->
            Just Reflexes

        "SAV" ->
            Just Savvy

        "SOM" ->
            Just Somatics

        "WIL" ->
            Just Willpower

        _ ->
            Nothing
