module EclipsePhase2.Styles exposing (..)

import Style exposing (StyleSheet, style)


type Styles
    = None


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None []
        ]
