module EclipsePhase2.Aptitude exposing (..)

-- PUBLIC --


type Aptitude
    = Cognition
    | Intuition
    | Reflexes
    | Savvy
    | Somatics
    | Willpower


default : Aptitude
default =
    Cognition


all : List ( String, Aptitude )
all =
    [ ( "Cognition", Cognition )
    , ( "Intuition", Intuition )
    , ( "Reflexes", Reflexes )
    , ( "Savvy", Savvy )
    , ( "Somatics", Somatics )
    , ( "Willpower", Willpower )
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
