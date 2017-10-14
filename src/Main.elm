module Main exposing (..)

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
    { skills : List String
    , newSkill : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { skills = []
      , newSkill = Nothing
      }
    , Cmd.none
    )


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
                    ( { model
                        | skills = List.sort <| skill :: model.skills
                        , newSkill = Nothing
                      }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div []
        [ ul [] (List.map (\s -> option [] [ text s ]) model.skills)
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
