module Main exposing (..)

import EclipsePhase2.Aptitude as Aptitude exposing (Aptitude)
import EclipsePhase2.Skill as Skill exposing (Skill)
import EclipsePhase2.Styles as Styles exposing (Styles)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Html exposing (Html)


type alias Model =
    { skills : List Skill
    , skillField : Maybe String
    , aptitudeMenu : Input.SelectWith Aptitude Msg
    , count : Int
    , skillSearch : Input.SelectWith Skill.Name Msg
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
    case Skill.nameFromString skill of
        Nothing ->
            Nothing

        Just name ->
            -- Referring to specific union type which should be abstracted
            Just (Skill.init name <| Skill.initInformation Skill.Know_ aptitude)


type Msg
    = NoOp
    | EditSkill String
    | AddSkill
    | Search (Input.SelectMsg Skill.Name)
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
            -- This is a mess...
            let
                field =
                    Maybe.withDefault "" model.skillField

                good =
                    not (Skill.requiresField selectedSkillName)
                        || (Skill.requiresField selectedSkillName && hasField field)

                selectedSkillName =
                    Maybe.withDefault Skill.defaultName <|
                        Input.selected model.skillSearch

                selectedAptitude =
                    -- Referring to specific union type which should be abstracted
                    case selectedSkillName of
                        Skill.Exotic _ ->
                            Maybe.withDefault
                                (Skill.linkedAptitude Aptitude.default selectedSkillName)
                            <|
                                Input.selected model.aptitudeMenu

                        name ->
                            Skill.linkedAptitude Aptitude.default name

                config =
                    { aptitude = selectedAptitude
                    , field = field
                    }

                updatedSkills =
                    model.skills
                        |> (++) [ (newSkill selectedSkillName config) ]
                        |> List.sortBy
                            (\(Skill.Skill name _) ->
                                Skill.nameToString name
                            )
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


hasField : Skill.Field -> Bool
hasField field =
    field
        |> String.trim
        |> String.isEmpty
        |> not


newSkill : Skill.Name -> { aptitude : Aptitude, field : Skill.Field } -> Skill
newSkill name config =
    let
        skillInfo =
            Skill.initInformation Skill.Know_ config.aptitude

        noField =
            Skill.init name skillInfo

        withField =
            Skill.init (Skill.nameFromField name config.field) skillInfo
    in
        case name of
            Skill.Exotic _ ->
                withField

            Skill.Hardware _ ->
                withField

            Skill.Know _ ->
                withField

            Skill.Medicine _ ->
                withField

            Skill.Pilot _ ->
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
viewSkill (Skill.Skill name info) =
    let
        skillText =
            Skill.nameToString name

        aptitudeText =
            Aptitude.toCode info.aptitude
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
                        , max = List.length Skill.all
                        , menu =
                            Input.menu Styles.None
                                []
                                (List.map viewChoice Skill.all)
                        }
                  ]
                , viewSkillFormInput model <|
                    Maybe.withDefault Skill.defaultName
                        (Input.selected model.skillSearch)
                , [ button Styles.None
                        [ onClick AddSkill
                        , alignLeft
                        ]
                        (text "Add Skill")
                  ]
                ]
            )


viewSkillFormInput : Model -> Skill.Name -> List (Element Styles variation Msg)
viewSkillFormInput model name =
    let
        aptitudeChoice ( _, aptitude ) =
            Input.choice aptitude (text <| Aptitude.toCode aptitude)

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
                                (List.map aptitudeChoice Aptitude.all)
                        }
                     )
                   ]
    in
        case name of
            Skill.Exotic _ ->
                withFieldAndAptitudes

            Skill.Hardware _ ->
                withField

            Skill.Know _ ->
                withField

            Skill.Medicine _ ->
                withField

            Skill.Pilot _ ->
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
