module EclipsePhase2.Styles exposing (..)

import Style exposing (StyleSheet, style)
import Style.Border as Border


type Styles
    = None
    | Border


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style Border
            [ Border.solid
            , Border.all 1
            ]
        ]
