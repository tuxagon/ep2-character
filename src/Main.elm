module Main exposing (..)

import Dict exposing (Dict)
import EclipsePhase2.Styles as Styles exposing (Styles)
import Element exposing (..)
import Element.Events as Evt
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
    , newSkill : Maybe String
    , count : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { skills = []
      , newSkill = Nothing
      , count = 0
      }
    , Cmd.none
    )


buildSkill : String -> Maybe Skill
buildSkill skill =
    case skillName skill of
        Nothing ->
            Nothing

        Just name ->
            Just <|
                Skill
                    name
                    { aptitude = Cognition, baseStat = 0, kind = Know, specializations = [] }


type Msg
    = NoOp
    | EditSkill String
    | AddSkill


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EditSkill skill ->
            ( { model
                | newSkill =
                    if skill == "" then
                        Nothing
                    else
                        Just skill
              }
            , Cmd.none
            )

        AddSkill ->
            case model.newSkill of
                Nothing ->
                    ( model, Cmd.none )

                Just skill ->
                    let
                        skill_ =
                            case buildSkill skill of
                                Nothing ->
                                    []

                                Just skill__ ->
                                    [ skill__ ]

                        updatedSkills =
                            List.sortBy (\(Skill name _) -> skillNameText name) <|
                                skill_
                                    ++ model.skills
                    in
                        ( { model
                            | skills = updatedSkills
                            , newSkill = Nothing
                            , count = model.count + 1
                          }
                        , Cmd.none
                        )


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
    let
        viewSkill (Skill name info) =
            el Styles.None
                []
                (text <| skillNameText name)
    in
        column Styles.None
            []
            (List.map viewSkill skills)


viewSkillForm : Model -> Element Styles variation Msg
viewSkillForm model =
    let
        text_ =
            case model.newSkill of
                Nothing ->
                    ""

                Just val ->
                    val
    in
        column Styles.None
            []
            [ Input.text Styles.None
                []
                { onChange = EditSkill
                , value = text_
                , label = Input.hiddenLabel "Skill"
                , options =
                    [ Input.textKey (toString model.count)
                    ]
                }
            , button Styles.None [ Evt.onClick AddSkill ] (text "Add Skill")
            ]



-- view : Model -> Html Msg
-- view model =
--     let
--         viewSkill (Skill name info) =
--             li [] [ text <| skillNameText name ]
--     in
--         div []
--             [ ul [] (List.map viewSkill model.skills)
--             , viewSkillForm model
--             , button [ onClick AddSkill ] [ text "Add Skill" ]
--             ]
-- viewSkillForm : Model -> Html Msg
-- viewSkillForm model =
--     let
--         viewSkillOption : ( String, SkillName ) -> Html Msg
--         viewSkillOption ( key, val ) =
--             option
--                 []
--                 [ text key ]
--     in
--         div []
--             [ select
--                 []
--                 (List.map viewSkillOption skillsMap)
--             , input [ onInput EditSkill ] []
--             ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


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
