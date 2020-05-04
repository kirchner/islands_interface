module Ui.Theme.Color exposing
    ( background
    , blueDark
    , blueLight
    , brownDark
    , brownLight
    , green
    , orange
    , transparent
    )

import Element exposing (Color)


background : Color
background =
    Element.rgb255 236 227 212


green : Color
green =
    Element.rgb255 86 116 66


brownDark : Color
brownDark =
    Element.rgb255 71 53 53


brownLight : Color
brownLight =
    Element.rgb255 135 108 87


blueLight : Color
blueLight =
    Element.rgb255 130 193 237


blueDark : Color
blueDark =
    Element.rgb255 80 150 235


orange : Color
orange =
    Element.rgb255 243 133 36


transparent : Color
transparent =
    Element.rgba255 255 255 255 0
