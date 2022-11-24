module Common.Events exposing (greedyOnClick, onMouseEnter, onMouseLeave)

import Element exposing (Attribute, htmlAttribute)
import Html.Events as HE
import Json.Decode as D


greedyOnClick : msg -> Attribute msg
greedyOnClick msg =
    htmlAttribute <|
        HE.custom "click" (D.succeed { message = msg, preventDefault = True, stopPropagation = True })


onMouseEnter : msg -> Attribute msg
onMouseEnter =
    htmlAttribute << HE.onMouseEnter


onMouseLeave : msg -> Attribute msg
onMouseLeave =
    htmlAttribute << HE.onMouseLeave
