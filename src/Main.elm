module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)


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
