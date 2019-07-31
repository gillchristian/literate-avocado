module Cx exposing
    ( body
    , content
    , controls
    , elmHot
    , empty
    , file
    , gistHeader
    , gistItem
    , gistItemGrid
    , gistItemLink
    , gistItemList
    , gistPrivateLabel
    , gists
    , gistsGird
    , gistsList
    , global
    , input
    , inputAndBtn
    , minW
    , notAskedMsg
    , searchBtn
    , searchInput
    , sidebar
    , sidebarBackdrop
    , sidebarCloseBtn
    , sidebarContent
    , sidebarHeader
    , sidebarOpen
    , sidebarOpenBtn
    , small
    , tokenBlock
    , tokenBlockHeading
    , tokenBlockMsg
    )

import Css exposing (..)
import Css.Global as G
import Css.Media as M
import Css.Transitions as Ts
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)


content : Html.Styled.Attribute msg
content =
    css
        [ displayFlex
        , flexDirection column
        , padding3 (px 50) (px 20) (px 20)
        ]


notAskedMsg : Html.Styled.Attribute msg
notAskedMsg =
    css [ marginTop <| px 20 ]


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
    css [ fontSize <| px 15, modifier ]


gistHeader : Html.Styled.Attribute msg
gistHeader =
    css [ displayFlex, alignItems center ]


gistItemGrid : Style
gistItemGrid =
    batch
        [ width <| px 280
        , padding4 (px 10) (px 20) (px 20) zero
        ]


gistItemList : Style
gistItemList =
    batch [ padding4 (px 20) zero zero zero ]


gistItemLink : Html.Styled.Attribute msg
gistItemLink =
    css
        [ color <| hex "#1A00F2"
        , whiteSpace noWrap
        , overflow hidden
        , display block
        , textOverflow ellipsis
        , marginRight <| px 3
        ]


gistPrivateLabel : Html.Styled.Attribute msg
gistPrivateLabel =
    css
        [ fontWeight bold
        , color <| hex "#AAA"
        , lineHeight <| int 1
        , fontSize <| px 12
        , borderRadius <| px 2
        ]


file : Html.Styled.Attribute msg
file =
    css [ marginTop <| px 5 ]


controls : Html.Styled.Attribute msg
controls =
    css [ displayFlex, flexWrap wrap ]


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
        , maxWidth <| vw 100
        , height <| vh 100
        , maxHeight <| vh 100
        , backgroundColor <| rgb 255 255 255
        , left zero
        , top zero
        , outline none
        , overflowX hidden
        , overflowY auto
        , position fixed
        , transform <| translateX <| pct -100
        , Ts.transition <|
            [ Ts.transform3 233.0 0 <| Ts.cubicBezier 0 0 0.21 1.0 ]
        , zIndex <| int 2000
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
        , backgroundColor <| rgba 0 0 0 0.5
        , zIndex <| int 1000
        ]


sidebarOpenBtn : Html.Styled.Attribute msg
sidebarOpenBtn =
    css
        [ position fixed
        , padding <| px 5
        , backgroundColor <| hex "#fff"
        , left <| px 15
        , top <| px 5
        , cursor pointer
        , fontSize <| px 26
        , lineHeight <| int 1
        , zIndex <| int 2000
        ]


sidebarCloseBtn : Html.Styled.Attribute msg
sidebarCloseBtn =
    css
        [ position absolute
        , right <| px 10
        , top <| px 5
        , cursor pointer
        , fontSize <| px 26
        , lineHeight <| int 1
        , zIndex <| int 2000
        ]


sidebarContent : Html.Styled.Attribute msg
sidebarContent =
    css
        [ position relative
        , padding4 (px 20) (px 20) zero (px 15)
        ]


sidebarHeader : Html.Styled.Attribute msg
sidebarHeader =
    css [ paddingLeft <| px 5, marginBottom <| px 50 ]


tokenBlock : Html.Styled.Attribute msg
tokenBlock =
    css []


tokenBlockMsg : Html.Styled.Attribute msg
tokenBlockMsg =
    css [ paddingLeft <| px 5, fontSize <| px 12, marginBottom <| px 5 ]


tokenBlockHeading : Html.Styled.Attribute msg
tokenBlockHeading =
    css [ paddingLeft <| px 5, fontWeight <| bold ]


empty : Style
empty =
    batch []


small : Html.Styled.Attribute msg
small =
    css
        [ fontSize Css.small
        , paddingTop <| px 10
        , paddingLeft <| px 5
        ]


minW : Html.Styled.Attribute msg
minW =
    css [ minWidth <| px 120 ]



-- GLOBAL --


global : Html msg
global =
    G.global
        [ elmHot
        , G.everything [ boxSizing borderBox ]
        , body
        , G.a [ textDecoration none ]
        , G.h1
            [ fontSize <| px 24
            , M.withMedia
                [ M.only M.screen [ M.maxWidth <| px 320 ] ]
                [ fontSize <| px 18 ]
            ]
        , G.p [ marginTop zero ]
        , G.img [ margin2 (px 20) zero, maxWidth <| px 200 ]
        , G.button [ border3 (px 2) solid (hex "#333") ]
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
        , borderBottom3 (px 2) solid (hex "#333")
        ]


inputAndBtn : G.Snippet
inputAndBtn =
    G.each [ G.input, G.button ]
        [ backgroundColor transparent
        , outline none
        , margin <| px 5
        , padding <| px 5
        , fontSize <| px 14
        , cursor pointer
        , disabled [ cursor notAllowed ]
        ]
