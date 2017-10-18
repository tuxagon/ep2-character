module EclipsePhase2.Skill exposing (..)

import Dict exposing (Dict)
import EclipsePhase2.Aptitude as Aptitude exposing (Aptitude)


-- PUBLIC --


type Category
    = Combat
    | Mental
    | Physical
    | Psi_
    | Social
    | Technical
    | Vehicle


type Kind
    = Know_
    | Active Category


type alias Field =
    String


type Name
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
    | Know Field
    | Medicine Field
    | Melee
    | Perceive
    | Persuade
    | Pilot Field
    | Program
    | Provoke
    | Psi
    | Research
    | Survival


type alias Information =
    { aptitude : Aptitude
    , baseStat : Int
    , kind : Kind
    , specializations : List String
    }


type Skill
    = Skill Name Information


defaultName : Name
defaultName =
    Athletics


init : Name -> Information -> Skill
init name info =
    Skill name info


initInformation : Kind -> Aptitude -> Information
initInformation kind aptitude =
    { aptitude = aptitude
    , baseStat = 0
    , kind = kind
    , specializations = []
    }


requiresField : Name -> Bool
requiresField name =
    case name of
        Exotic _ ->
            True

        Hardware _ ->
            True

        Know _ ->
            True

        Medicine _ ->
            True

        Pilot _ ->
            True

        _ ->
            False


all : List ( String, Name )
all =
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
    , ( "Know", Know "" )
    , ( "Medicine", Medicine "" )
    , ( "Melee", Melee )
    , ( "Perceive", Perceive )
    , ( "Persuade", Persuade )
    , ( "Pilot", Pilot "" )
    , ( "Program", Program )
    , ( "Provoke", Provoke )
    , ( "Psi", Psi )
    , ( "Research", Research )
    , ( "Survival", Survival )
    ]


nameFromString : String -> Maybe Name
nameFromString key =
    all
        |> List.map (\( k, v ) -> ( String.toLower k, v ))
        |> Dict.fromList
        |> Dict.get (String.toLower key)


nameToString : Name -> String
nameToString name =
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

        Know knowField ->
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

        Psi ->
            "Psi"

        Research ->
            "Research"

        Survival ->
            "Survival"


linkedAptitude : Aptitude -> Name -> Aptitude
linkedAptitude default name =
    case name of
        Athletics ->
            Aptitude.Somatics

        Deceive ->
            Aptitude.Savvy

        Exotic _ ->
            default

        Fray ->
            Aptitude.Reflexes

        FreeFall ->
            Aptitude.Somatics

        Guns ->
            Aptitude.Reflexes

        Hardware _ ->
            Aptitude.Cognition

        Infiltrate ->
            Aptitude.Reflexes

        Infosec ->
            Aptitude.Cognition

        Interface ->
            Aptitude.Cognition

        Kinesics ->
            Aptitude.Savvy

        Know _ ->
            Aptitude.Cognition

        Medicine _ ->
            Aptitude.Cognition

        Melee ->
            Aptitude.Somatics

        Perceive ->
            Aptitude.Intuition

        Persuade ->
            Aptitude.Savvy

        Pilot _ ->
            Aptitude.Reflexes

        Program ->
            Aptitude.Cognition

        Provoke ->
            Aptitude.Savvy

        Psi ->
            Aptitude.Willpower

        Research ->
            Aptitude.Intuition

        Survival ->
            Aptitude.Intuition


nameFromField : Name -> Field -> Name
nameFromField name field =
    case name of
        Exotic _ ->
            Exotic field

        Hardware _ ->
            Hardware field

        Know _ ->
            Know field

        Medicine _ ->
            Medicine field

        Pilot _ ->
            Pilot field

        _ ->
            name
