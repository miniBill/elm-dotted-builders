module Types exposing (..)

{-| Hues! -}
type Hue
    = Primary
    | Secondary
    | Danger


type Target
    = Background
    | Font


type Opacity
    = Op100
    | Op70
    | Op30


type Accent
    = Normal
    | Subtle


type Color
    = Color Target Opacity Accent Hue

type SimpleColor
    = JustHue Hue
    | HueWithOpacity Hue Opacity
