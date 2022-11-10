module Palette.Utils exposing (..)

import Element exposing (..)
import FontAwesome as FA
import FontAwesome.Solid as FA


displayIcon : FA.Icon hasId -> Element msg
displayIcon =
    html << FA.view
