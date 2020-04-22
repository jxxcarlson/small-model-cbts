module Style exposing
    ( button
    , charcoal
    , controlPanel
    , dashboard
    , endColor
    , lightBlue
    , lightColor
    , log
    , mainColumn
    , paleBlue
    , pausedColor
    , rowA
    , rowB
    , rowX
    , rowY
    , selectedButton
    , titleColor
    , whiteColor
    )

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input


mainColumn =
    [ Background.color (rgb255 100 100 120)
    , spacing 10
    , width fill
    , height fill
    , paddingXY 20 0
    ]


dashboard =
    [ Background.color (rgb255 200 200 200)
    , paddingXY 12 18
    , width (px 300)
    , height (px 570)
    , spacing 6
    , Font.size 14
    , Font.family [ Font.typeface "Courier" ]
    ]


controlPanel =
    [ Background.color (rgb255 200 200 200)
    , paddingXY 12 18
    , width (px 150)
    , height (px 570)
    , spacing 6
    , Font.size 14
    , Font.family [ Font.typeface "Courier" ]
    ]


log =
    [ Background.color (rgb255 200 200 200)
    , paddingXY 12 18
    , width (px 330)
    , height (px 570)
    , spacing 3
    , Font.size 14
    , Font.family [ Font.typeface "Courier" ]
    , scrollbarY
    ]


button =
    let
        g =
            80
    in
    [ Background.color (rgb255 g g g)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]


selectedButton =
    [ Background.color (rgb255 140 0 0)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]


charcoal : Color
charcoal =
    rgb255 40 40 40


lightColor : Color
lightColor =
    rgb255 200 200 200


whiteColor : Color
whiteColor =
    rgb255 220 220 220


rowA : Color
rowA =
    rgb255 255 245 182


rowX : Color
rowX =
    rgb255 255 100 182


rowY : Color
rowY =
    rgb255 100 182 255


rowB : Color
rowB =
    rgb255 239 192 112


lightBlue : Color
lightBlue =
    rgb255 90 90 255


paleBlue : Color
paleBlue =
    rgb255 190 190 255


pausedColor : Color
pausedColor =
    rgb255 255 255 200


endColor : Color
endColor =
    rgb255 255 200 200


titleColor : Color
titleColor =
    rgb255 190 190 255
