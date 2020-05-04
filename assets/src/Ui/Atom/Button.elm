module Ui.Atom.Button exposing
    ( Button, button
    , toElement
    , Role(..), withRole
    , focusedOnLoad
    )

{-|

@docs Button, button
@docs toElement
@docs Role, withRole
@docs focusedOnLoad

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Ui.Theme.Color
import Ui.Theme.Spacing


{-| -}
type Button msg
    = Button Options msg String


{-| -}
button : msg -> String -> Button msg
button onPress label =
    Button defaultOptions onPress label


type alias Options =
    { focusedOnLoad : Bool
    , role : Role
    }


defaultOptions : Options
defaultOptions =
    { focusedOnLoad = False
    , role = Primary
    }


{-| -}
type Role
    = Primary
    | Secondary


{-| -}
withRole : Role -> Button msg -> Button msg
withRole role (Button options onPress label) =
    Button { options | role = role } onPress label


{-| -}
focusedOnLoad : Button msg -> Button msg
focusedOnLoad (Button options onPress label) =
    Button { options | focusedOnLoad = True } onPress label


{-| -}
toElement : Button msg -> Element msg
toElement (Button options onPress label) =
    let
        addFocusedOnLoad attributes =
            if options.focusedOnLoad then
                Input.focusedOnLoad :: attributes

            else
                attributes
    in
    Input.button
        ([ Background.color Ui.Theme.Color.orange
         , Element.width Element.fill
         , Border.rounded 6
         ]
            |> addFocusedOnLoad
        )
        { onPress = Just onPress
        , label =
            Element.el
                [ Element.centerX
                , Element.padding Ui.Theme.Spacing.level2
                ]
                (Element.text label)
        }
