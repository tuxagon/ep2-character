module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Events exposing (..)


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
    }


init : ( Model, Cmd Msg )
init =
    ( { skills = []
      , newSkill = Nothing
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
                          }
                        , Cmd.none
                        )


view : Model -> Html Msg
view model =
    let
        viewSkill (Skill name info) =
            li [] [ text <| skillNameText name ]
    in
        div []
            [ ul [] (List.map viewSkill model.skills)
            , input [ onInput EditSkill ] []
            , button [ onClick AddSkill ] [ text "Add Skill" ]
            ]


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
