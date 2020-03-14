port module Main exposing (andMap, main)

import Browser
import ConfigField exposing (ConfigField(..))
import Cx
import Dict as Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( disabled
        , href
        , placeholder
        , src
        , target
        , title
        , type_
        , value
        )
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http exposing (Error(..))
import Json.Decode as D
import Json.Decode.Extra as D
import Json.Encode as E
import List as List
import Maybe as Maybe
import Maybe.Extra as Maybe
import Parser as P exposing ((|.), (|=))
import Platform.Cmd as Cmd
import RemoteData as RemoteData exposing (RemoteData(..), WebData)
import Result
import Set exposing (Set)
import String
import Time
import Tuple exposing (first, second)



---- MODEL ----


type alias User =
    { id : Int
    , login : String
    , html_url : String
    , avatar_url : String
    }


type alias File =
    { filename : String
    , language : Maybe String
    }


type alias RawGist =
    { id : String
    , description : Maybe String
    , html_url : String
    , files : Dict String File
    , public : Bool
    , created_at : Time.Posix
    , updated_at : Time.Posix
    , owner : User
    }


type alias Gist =
    { id : String
    , description : Maybe String
    , html_url : String
    , files : Dict String File
    , public : Bool
    , created_at : Time.Posix
    , updated_at : Time.Posix
    , owner : User
    , tags : Set String
    }


type Display
    = Grid
    | List


type Visible
    = Show
    | Hide


type alias PersistedConfig =
    { username : Maybe String
    , token : Maybe String
    }


type alias Token =
    ConfigField String


type alias Model =
    { gists : WebData (List Gist)
    , display : Display
    , showFiles : Bool
    , username : Maybe String
    , token : Token
    , search : String
    , sidebar : Visible
    }


authHeader : Maybe String -> Maybe Http.Header
authHeader =
    Maybe.map (Http.header "Authorization" << (++) "token ")


type alias Headers =
    Dict String String


expectJsonWithHeaders :
    (Result Http.Error ( a, Headers ) -> msg)
    -> D.Decoder a
    -> Http.Expect msg
expectJsonWithHeaders toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ { headers } body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok ( value, headers )

                        Err err ->
                            Err (Http.BadBody (D.errorToString err))


getGists : Maybe String -> String -> Cmd Msg
getGists token url =
    Http.request
        { method = "GET"
        , headers = Maybe.toList <| authHeader token
        , url = url
        , body = Http.emptyBody
        , expect =
            expectJsonWithHeaders
                (RemoteData.fromResult >> GotGists)
                (D.list gistD)
        , timeout = Nothing
        , tracker = Nothing
        }


init : ( Model, Cmd Msg )
init =
    ( { gists = NotAsked
      , display = Grid
      , showFiles = False
      , username = Nothing
      , token = Empty
      , search = ""
      , sidebar = Hide
      }
    , doLoadFromStorage ()
    )



---- -> Parsing -> ----


tagsP : P.Parser (List String)
tagsP =
    P.succeed identity
        |. P.chompWhile ((/=) '[')
        |= tagsListP


tagsListP : P.Parser (List String)
tagsListP =
    P.sequence
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = P.spaces
        , item = tagP
        , trailing = P.Optional
        }


tagP : P.Parser String
tagP =
    P.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '-'
        , reserved = Set.empty
        }


{-| Cleans up the description by removing the tags list (all what's inside `[]`).

    "Some text [tag, tag] and more text" -> "Some text and more text"

-}
descriptionP : P.Parser String
descriptionP =
    P.map String.trim <|
        P.succeed (\left right -> left ++ " " ++ right)
            |= (P.getChompedString <| P.chompWhile ((/=) '['))
            |. P.spaces
            |. P.chompWhile ((/=) ']')
            |. P.chompIf ((==) ']')
            |. P.spaces
            |= (P.getChompedString <| P.chompWhile (always True))



---- -> JSON -> ----


{-| Poor's man applicative:

    f : A -> B -> C

    deoderA : Decoder A

    deoderB : Decoder B

    c : Decoder C
    c =
        D.succeed f
            |> andMap deoderA
            |> andMap deoderB

-}
andMap : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
andMap =
    D.map2 (|>)


gistD : D.Decoder RawGist
gistD =
    D.succeed RawGist
        |> andMap (D.field "id" D.string)
        |> andMap (D.field "description" <| D.nullable D.string)
        |> andMap (D.field "html_url" D.string)
        |> andMap (D.field "files" <| D.dict fileD)
        |> andMap (D.field "public" D.bool)
        |> andMap (D.field "created_at" D.datetime)
        |> andMap (D.field "updated_at" D.datetime)
        |> andMap (D.field "owner" userD)


userD : D.Decoder User
userD =
    D.succeed User
        |> andMap (D.field "id" D.int)
        |> andMap (D.field "login" D.string)
        |> andMap (D.field "html_url" D.string)
        |> andMap (D.field "avatar_url" D.string)


fileD : D.Decoder File
fileD =
    D.succeed File
        |> andMap (D.field "filename" D.string)
        |> andMap (D.field "language" <| D.nullable D.string)


persistedD : D.Decoder PersistedConfig
persistedD =
    D.succeed PersistedConfig
        |> andMap (D.field "username" <| D.nullable D.string)
        |> andMap (D.field "token" <| D.nullable D.string)


persistedE : PersistedConfig -> E.Value
persistedE { username, token } =
    E.object
        [ ( "username", maybeE E.string username )
        , ( "token", maybeE E.string token )
        ]


maybeE : (a -> E.Value) -> Maybe a -> E.Value
maybeE encoder =
    Maybe.unwrap E.null encoder


decodePersitedConfig : D.Value -> PersistedConfig
decodePersitedConfig =
    Result.withDefault { username = Nothing, token = Nothing }
        << D.decodeValue persistedD



---- UPDATE ----


type Msg
    = GotGists (WebData ( List RawGist, Headers ))
    | LoadFromStorage PersistedConfig
      -- Search
    | ChangeSearch String
    | SearchGists
      -- Token
    | ChangeToken String
    | CancelEditingToken
    | ClearToken
    | AddNewToken
    | SaveToken String
      -- UI
    | ChangeDisplay Display
    | ToggleFiles
    | ToggleSidebar


{-| nextUrl parses the "Link" header from GitHub's API to get the next url.
Docs: [developer.github.com/v3/gists](https://developer.github.com/v3/gists).

Transforms this:

    <https://api.github.com/user/8309423/gists?page=2>; rel="next",
    <https://api.github.com/user/8309423/gists?page=2>; rel="last"

    <https://api.github.com/user/8309423/gists?page=1>; rel="prev",
    <https://api.github.com/user/8309423/gists?page=3>; rel="next",
    <https://api.github.com/user/8309423/gists?page=5>; rel="last",
    <https://api.github.com/user/8309423/gists?page=1>; rel="first"

To this:

    https://api.github.com/user/8309423/gists?page=2

    https://api.github.com/user/8309423/gists?page=3

-}
nextUrl : Headers -> Maybe String
nextUrl =
    Dict.get "link"
        >> Maybe.filter hasNext
        >> Maybe.map (String.split "," >> List.filter hasNext)
        >> Maybe.andThen List.head
        >> Maybe.map (String.split ";")
        >> Maybe.andThen List.head
        >> Maybe.map (String.replace "<" "" >> String.replace ">" "")


hasNext : String -> Bool
hasNext =
    String.contains "rel=\"next\""


parseTags : RawGist -> Gist
parseTags gist =
    let
        tags =
            gist.description
                |> Maybe.andThen (P.run tagsP >> Result.toMaybe)
                |> Maybe.withDefault []
                |> Set.fromList

        description =
            Maybe.andThen (P.run descriptionP >> Result.toMaybe) gist.description
    in
    { id = gist.id
    , description = description
    , html_url = gist.html_url
    , files = gist.files
    , public = gist.public
    , created_at = gist.created_at
    , updated_at = gist.updated_at
    , owner = gist.owner
    , tags = tags
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGists result ->
            let
                appendNewGists =
                    (++) <| RemoteData.withDefault [] model.gists

                gists =
                    RemoteData.map (first >> List.map parseTags >> appendNewGists) result
            in
            ( { model | gists = gists, search = "" }
            , Maybe.unwrap
                Cmd.none
                (getGists <| ConfigField.toMaybe model.token)
              <|
                RemoteData.unwrap Nothing (nextUrl << second) result
            )

        LoadFromStorage { username, token } ->
            ( { model
                | username = username
                , token = ConfigField.fromMaybe token
                , gists = Maybe.unwrap NotAsked (always Loading) username
              }
            , Maybe.unwrap Cmd.none (getGists token << gistsUrl) username
            )

        -- Search
        ChangeSearch name ->
            ( { model | search = name }, Cmd.none )

        SearchGists ->
            if model.search == "" then
                ( model, Cmd.none )

            else
                ( { model | gists = Loading, username = Just model.search }
                , Cmd.batch
                    [ getGists
                        (ConfigField.toMaybe model.token)
                        (gistsUrl model.search)
                    , saveToStorage <|
                        persistedE
                            { token = ConfigField.toMaybe model.token
                            , username = Just model.search
                            }
                    ]
                )

        -- Token
        ChangeToken token ->
            ( { model | token = Editing token }
            , Cmd.none
            )

        CancelEditingToken ->
            ( { model | token = Empty }
            , Cmd.none
            )

        ClearToken ->
            ( { model
                | token = Empty
                , gists = Maybe.unwrap NotAsked (always Loading) model.username
              }
            , Cmd.batch
                [ Maybe.unwrap
                    Cmd.none
                    (getGists Nothing << gistsUrl)
                    model.username
                , saveToStorage <|
                    persistedE { token = Nothing, username = model.username }
                ]
            )

        AddNewToken ->
            ( { model | token = Editing "" }
            , Cmd.none
            )

        SaveToken token ->
            if token == "" then
                ( model, Cmd.none )

            else
                ( { model
                    | token = Saved token
                    , gists = Maybe.unwrap NotAsked (always Loading) model.username
                  }
                , Cmd.batch
                    [ Maybe.unwrap
                        Cmd.none
                        (getGists (Just token) << gistsUrl)
                        model.username
                    , saveToStorage <|
                        persistedE
                            { token = Just token, username = model.username }
                    ]
                )

        -- UI ----------------------------------------------
        ChangeDisplay display ->
            case display of
                Grid ->
                    ( { model | display = display, showFiles = False }
                    , Cmd.none
                    )

                List ->
                    ( { model | display = display, showFiles = True }
                    , Cmd.none
                    )

        ToggleFiles ->
            ( { model | showFiles = not model.showFiles }, Cmd.none )

        ToggleSidebar ->
            ( { model | sidebar = showHide Hide Show model.sidebar }, Cmd.none )


gistsUrl : String -> String
gistsUrl username =
    "https://api.github.com/users/" ++ username ++ "/gists"



---- VIEW ----


showHide : a -> a -> Visible -> a
showHide onShow onHide x =
    case x of
        Show ->
            onShow

        Hide ->
            onHide


view : Model -> Html Msg
view model =
    div [ Cx.site ]
        [ renderContent model
        , renderFooter
        ]


renderContent : Model -> Html Msg
renderContent model =
    div [ Cx.content ]
        [ Cx.global
        , renderControls model
        , renderTitle model
        , renderTokenMsg model
        , renderGists model
        , div [ Cx.sidebarOpenBtn, onClick ToggleSidebar ] [ text "☰" ]
        , sidebar model.sidebar <| renderSidebarControls model
        , showHide
            (div [ Cx.sidebarBackdrop, onClick ToggleSidebar ] [])
            (text "")
            model.sidebar
        ]


renderFooter : Html Msg
renderFooter =
    footer []
        [ div []
            [ text " by "
            , a
                [ href "https://twitter.com/gillchristian"
                , target "_blank"
                ]
                [ text "@gillchristian" ]
            , text " <> code on "
            , a
                [ href "https://github.com/gillchristian/literate-avocado"
                , target "_blank"
                ]
                [ text "GitHub" ]
            ]
        ]


githubGistAuthDocs : String
githubGistAuthDocs =
    "https://developer.github.com/v3/gists/#authentication"


githubToken : String
githubToken =
    "https://github.com/settings/tokens"


renderTokenMsg : Model -> Html Msg
renderTokenMsg { token, gists } =
    case ( token, gists ) of
        ( Saved _, _ ) ->
            text ""

        ( _, Success _ ) ->
            p [ Cx.small ]
                [ text "Want to see your secret gists as well? Add a "
                , a
                    [ href githubToken, target "_blank" ]
                    [ text "GitHub token" ]
                , text " on the menu"
                ]

        _ ->
            text ""


renderSidebarControls : Model -> Html Msg
renderSidebarControls model =
    div []
        [ div [ Cx.sidebarCloseBtn, onClick ToggleSidebar ] [ text "❌" ]
        , div [ Cx.sidebarContent ]
            [ div [ Cx.sidebarHeader ] [ h2 [] [ text "literate-avocado" ] ]
            , renderTokenBlock model
            ]
        ]


renderTokenBlock : Model -> Html Msg
renderTokenBlock model =
    div [ Cx.tokenBlock ]
        [ p [ Cx.tokenBlockHeading ] [ text "GitHub Token" ]
        , p [ Cx.tokenBlockMsg ]
            [ text "Only required to search secret Gists. "
            , text "No extra scopes are necessary, only read access is used. "
            ]
        , p [ Cx.tokenBlockMsg ]
            [ text "Check "
            , a [ href githubGistAuthDocs, target "_blank" ] [ text "the docs" ]
            , text " to learn more."
            ]
        , case model.token of
            Empty ->
                button
                    [ Cx.searchBtn, type_ "button", onClick AddNewToken ]
                    [ text "Add Token" ]

            Editing token ->
                form [ onSubmit <| SaveToken token ]
                    [ input
                        [ Cx.searchInput
                        , placeholder "a8i7hov674dbq15nm09"
                        , onInput ChangeToken
                        , value token
                        ]
                        []
                    , button
                        [ Cx.searchBtn
                        , type_ "submit"
                        , disabled <| token == ""
                        ]
                        [ text "Save" ]
                    , button
                        [ Cx.searchBtn
                        , type_ "button"
                        , onClick <| CancelEditingToken
                        ]
                        [ text "Cancel" ]
                    ]

            Saved _ ->
                div []
                    [ button
                        [ Cx.searchBtn, type_ "button", onClick AddNewToken ]
                        [ text "Change" ]
                    , button
                        [ Cx.searchBtn, type_ "button", onClick ClearToken ]
                        [ text "Remove" ]
                    ]
        ]


sidebar : Visible -> Html Msg -> Html Msg
sidebar visible content =
    div
        [ Cx.sidebar <| showHide Cx.sidebarOpen Cx.empty visible ]
        [ showHide content (text "") visible ]


renderTitle : Model -> Html Msg
renderTitle { username, gists } =
    case username of
        Just name ->
            h1 []
                [ text <| name ++ " gists"
                , RemoteData.unwrap (text "")
                    (small []
                        << (\s -> [ text <| " (" ++ s ++ ")" ])
                        << String.fromInt
                        << List.length
                    )
                    gists
                ]

        Nothing ->
            text ""


renderControls : Model -> Html Msg
renderControls model =
    div [ Cx.controls ]
        [ form [ onSubmit SearchGists ]
            [ input
                [ Cx.searchInput
                , placeholder "GitHub username"
                , onInput ChangeSearch
                , value model.search
                , disabled <| RemoteData.isLoading model.gists
                ]
                []
            , button
                [ Cx.searchBtn, disabled <| model.search == "", type_ "submit" ]
                [ text "Search" ]
            ]
        , RemoteData.isSuccess model.gists
            |> not
            |> renderToggleDisplayBtn model.display
        , renderToggleFiles model
        ]


renderToggleDisplayBtn : Display -> Bool -> Html Msg
renderToggleDisplayBtn display disable =
    let
        ( msg, label ) =
            case display of
                Grid ->
                    ( ChangeDisplay List, "☷" )

                List ->
                    ( ChangeDisplay Grid, "☰" )
    in
    button [ onClick msg, disabled disable ] [ text label ]


renderToggleFiles : Model -> Html Msg
renderToggleFiles { showFiles, gists, display } =
    case display of
        Grid ->
            text ""

        List ->
            let
                label =
                    if showFiles then
                        "Only main file"

                    else
                        "All files"
            in
            button
                [ Cx.minW
                , onClick ToggleFiles
                , RemoteData.isSuccess gists |> not |> disabled
                ]
                [ text label ]


renderError : Http.Error -> Html Msg
renderError err =
    case err of
        BadUrl str ->
            p [] [ text str ]

        Timeout ->
            p [] [ text "Request timed out." ]

        NetworkError ->
            div []
                [ p [] [ text "Looks like you are offline." ]
                , p [] [ text "Check your connection and try again." ]
                ]

        BadStatus code ->
            p [] [ text <| "Status: " ++ String.fromInt code ]

        BadBody msg ->
            p [] [ text <| "BadBody: " ++ msg ]


renderGists : Model -> Html Msg
renderGists { display, showFiles, gists } =
    let
        ( styles, renderGist ) =
            case display of
                Grid ->
                    ( Cx.gists Cx.gistsGird, renderGridGist )

                List ->
                    ( Cx.gists Cx.gistsList, renderListGist display showFiles )
    in
    case gists of
        Success gs ->
            div
                [ styles ]
                (List.map renderGist gs)

        Failure err ->
            div [] [ renderError err ]

        Loading ->
            div [] [ text "..." ]

        NotAsked ->
            div [ Cx.notAskedMsg ] [ text "Search GitHub Gists by username" ]


renderListGist : Display -> Bool -> Gist -> Html Msg
renderListGist display showFiles gist =
    let
        files =
            Dict.values gist.files

        -- `gistName` is also the name of the first file (unless there's none)
        gistName =
            Maybe.unwrap gist.id .filename <| List.head files
    in
    div [ Cx.gistItem Cx.gistItemList ]
        [ div [ Cx.gistHeader ]
            [ div [ Cx.gistItemSection ]
                [ text <| formatTime Time.utc gist.updated_at ]
            , div [ Cx.gistItemSection ] [ text "|" ]
            , div [ Cx.gistItemSection ]
                [ a
                    [ Cx.gistItemLink
                    , href gist.html_url
                    , target "_blank"
                    , title gistName
                    ]
                    [ renderPrivateLabel gist.public, text gistName ]
                ]
            ]
        , div []
            [ gist.description
                |> Maybe.filter ((/=) "")
                |> Maybe.unwrap (text "") text
            ]
        , Set.toList gist.tags
            |> List.map renderTag
            |> div [ Cx.tags ]
        , renderFiles showFiles files
        ]


renderTag : String -> Html Msg
renderTag tagName =
    a [ Cx.tag ] [ text tagName ]


renderGridGist : Gist -> Html Msg
renderGridGist gist =
    let
        -- `gistName` is also the name of the first file (unless there's none)
        gistName =
            gist.files
                |> Dict.values
                |> List.head
                |> Maybe.unwrap gist.id .filename
    in
    div [ Cx.gistItem Cx.gistItemGrid ]
        [ div [ Cx.gistHeader ]
            [ a
                [ Cx.gistItemLink
                , href gist.html_url
                , target "_blank"
                , title gistName
                ]
                [ text gistName ]
            , renderPrivateLabel gist.public
            ]
        ]


renderPrivateLabel : Bool -> Html Msg
renderPrivateLabel public =
    if public then
        text ""

    else
        span [ Cx.gistPrivateLabel ] [ text "🔒" ]


renderFiles : Bool -> List File -> Html Msg
renderFiles showFiles files =
    if showFiles then
        files
            |> List.map renderFile
            |> div []

    else
        text ""


renderFile : File -> Html Msg
renderFile file =
    div [ Cx.file ] [ text file.filename ]


formatTime : Time.Zone -> Time.Posix -> String
formatTime z t =
    (Time.toYear z >> String.fromInt) t
        ++ "-"
        ++ (Time.toMonth z >> monthToString) t
        ++ "-"
        ++ (Time.toDay z >> intToString) t
        ++ " "
        ++ (Time.toHour z >> intToString) t
        ++ ":"
        ++ (Time.toMinute z >> intToString) t


intToString : Int -> String
intToString x =
    if x >= 10 then
        String.fromInt x

    else
        "0" ++ String.fromInt x


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"



---- SUBSCRIPTIONS ----


subscriptions =
    loadFromStorage (LoadFromStorage << decodePersitedConfig)


port saveToStorage : E.Value -> Cmd msg


port loadFromStorage : (D.Value -> msg) -> Sub msg


port doLoadFromStorage : () -> Cmd msg



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> subscriptions
        }
