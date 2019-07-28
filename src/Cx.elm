module Cx exposing
    ( body
    , content
    , elmHot
    , empty
    , gistItem
    , gistItemGrid
    , gistItemLink
    , gistItemList
    , gists
    , gistsGird
    , gistsList
    , global
    , input
    , inputAndBtn
    , menuToggle
    , search
    , searchBtn
    , searchInput
    , sidebar
    , sidebarBackdrop
    , sidebarOpen
    )

import Css exposing (..)
import Css.Global as G
import Css.Transitions as Ts
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)


content : Html.Styled.Attribute msg
content =
    css
        [ displayFlex
        , flexDirection column
        , padding <| px 20
        ]


gists : Style -> Html.Styled.Attribute msg
gists modifier =
    css
        [ displayFlex
        , width <| vw 90
        , modifier
        ]


gistsGird : Style
gistsGird =
    batch [ flexWrap wrap ]


gistsList : Style
gistsList =
    batch [ flexDirection column ]


gistItem : Style -> Html.Styled.Attribute msg
gistItem modifier =
    css [ fontSize <| px 13, modifier ]


gistItemGrid : Style
gistItemGrid =
    batch
        [ width <| px 230
        , padding4 (px 10) (px 10) (px 20) zero
        ]


gistItemList : Style
gistItemList =
    batch [ padding4 (px 10) zero zero zero ]


gistItemLink : Html.Styled.Attribute msg
gistItemLink =
    css
        [ color <| hex "#1A00F2"
        , whiteSpace noWrap
        , overflow hidden
        , display block
        , textOverflow ellipsis
        ]


search : Html.Styled.Attribute msg
search =
    css [ displayFlex ]


searchInput : Html.Styled.Attribute msg
searchInput =
    css []


searchBtn : Html.Styled.Attribute msg
searchBtn =
    css []


sidebar : Style -> Html.Styled.Attribute msg
sidebar modifier =
    css
        [ position fixed
        , width <| px 500
        , height <| vh 100
        , maxHeight <| vh 100
        , left zero
        , top zero
        , outline none
        , overflowX hidden
        , overflowY auto
        , padding2 zero (px 15)
        , position fixed
        , transform <| translateX <| pct -100
        , Ts.transition <|
            [ Ts.transform3 233.0 0 <| Ts.cubicBezier 0 0 0.21 1.0 ]
        , zIndex <| int 2000
        , backgroundColor <| rgb 255 255 255
        , modifier
        ]


sidebarOpen : Style
sidebarOpen =
    batch [ transform <| translateX zero ]


sidebarBackdrop : Html.Styled.Attribute msg
sidebarBackdrop =
    css
        [ position fixed
        , top zero
        , left zero
        , width <| vw 100
        , height <| vh 100

        -- TODO: add transition to the color
        , backgroundColor <| rgba 0 0 0 0.5
        , zIndex <| int 1000
        ]


menuToggle : Html.Styled.Attribute msg
menuToggle =
    css
        [ position fixed
        , right <| px 10
        , top <| px 10
        , cursor pointer
        , fontSize <| px 26
        , zIndex <| int 2000
        ]


empty : Style
empty =
    batch []



-- GLOBAL --


global : Html msg
global =
    G.global
        [ elmHot
        , body
        , G.a [ textDecoration none ]
        , G.h1 [ fontSize <| px 30 ]
        , G.img [ margin2 (px 20) zero, maxWidth <| px 200 ]
        , G.button [ border3 (px 3) solid (hex "#333") ]
        , input
        , inputAndBtn
        ]



{-
   elm-hot creates an additional div wrapper around the app to make HMR possible.
   This could break styling in development mode if you are using Elm UI.
   More context in the issue: https://github.com/halfzebra/create-elm-app/issues/320
-}


elmHot : G.Snippet
elmHot =
    G.selector "[data-elm-hot=\"true\"]" [ height inherit ]


body : G.Snippet
body =
    G.body
        [ margin zero
        , padding zero
        , color <| hex "#293c4b"
        , fontFamilies [ "Menlo", "Consolas", .value monospace ]
        , property "-webkit-font-smoothing" "antialiased"
        ]


input : G.Snippet
input =
    G.input
        [ border zero
        , borderBottom3 (px 3) solid (hex "#333")
        ]


inputAndBtn : G.Snippet
inputAndBtn =
    G.each [ G.input, G.button ]
        [ backgroundColor transparent
        , outline none
        , margin <| px 10
        , padding <| px 10
        , fontSize <| px 18
        , cursor pointer
        ]
