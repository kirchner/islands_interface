module Ui.Theme.Typography exposing (heading)

import Element exposing (Element)
import Element.Font as Font
import Element.Region as Region


heading : String -> Element msg
heading text =
    Element.el
        [ Region.heading 1
        , Element.centerX
        , Font.size 36
        ]
        (Element.text text)
