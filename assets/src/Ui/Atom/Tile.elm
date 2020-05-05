module Ui.Atom.Tile exposing
    ( Tile, tile
    , toElement
    , enabledWith
    , Role(..), withRole
    )

{-|

@docs Tile, tile
@docs toElement
@docs enabledWith
@docs Role, withRole

-}

import Element exposing (Device, DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Ui.Theme.Color
import Ui.Theme.Spacing


{-| -}
type Tile msg
    = Tile (Options msg) Device


type alias Options msg =
    { onPress : Maybe msg
    , role : Role
    }


defaultOptions : Options msg
defaultOptions =
    { onPress = Nothing
    , role = None
    }


{-| -}
enabledWith : msg -> Tile msg -> Tile msg
enabledWith onPress (Tile options device) =
    Tile { options | onPress = Just onPress } device


{-| -}
type Role
    = None
    | Island
    | Forest
    | Water
    | DiscoveredWater
    | Unknown


{-| -}
withRole : Role -> Tile msg -> Tile msg
withRole role (Tile options device) =
    Tile { options | role = role } device


{-| -}
tile : Device -> Tile msg
tile device =
    Tile defaultOptions device


{-| -}
toElement : Tile msg -> Element msg
toElement (Tile options device) =
    let
        attributes =
            case device.class of
                Phone ->
                    [ Element.width (Element.px Ui.Theme.Spacing.level3)
                    , Element.height (Element.px Ui.Theme.Spacing.level3)
                    , Background.color color
                    , Border.rounded 3
                    ]

                _ ->
                    [ Element.width (Element.px Ui.Theme.Spacing.level4)
                    , Element.height (Element.px Ui.Theme.Spacing.level4)
                    , Background.color color
                    , Border.rounded 3
                    ]

        color =
            case options.role of
                None ->
                    Ui.Theme.Color.transparent

                Island ->
                    Ui.Theme.Color.brownLight

                Forest ->
                    Ui.Theme.Color.green

                Water ->
                    Ui.Theme.Color.blueLight

                DiscoveredWater ->
                    Ui.Theme.Color.blueDark

                Unknown ->
                    Ui.Theme.Color.brownDark
    in
    case options.onPress of
        Nothing ->
            Element.el attributes Element.none

        Just onPress ->
            Input.button attributes
                { onPress = Just onPress
                , label = Element.none
                }
