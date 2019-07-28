port module Main exposing (main)

import Browser
import Cx
import Dict as Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (href, placeholder, src, target, title, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http exposing (Error(..))
import Json.Decode as D
import Json.Encode as E
import List as List
import Maybe as Maybe
import Maybe.Extra as Maybe
import Platform.Cmd as Cmd
import RemoteData exposing (RemoteData(..), WebData)
import String as String
import Tuple exposing (second)



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


type alias Gist =
    { id : String
    , description : Maybe String
    , html_url : String
    , files : Dict String File
    , public : Bool
    , created_at : String
    , updated_at : String
    , owner : User
    }


type Display
    = Grid
    | List



-- TODO: load user from localStorage (mvp)
-- TODO: modal with config: secret (mvp)
-- TODO: parse headers with next & fetch all pages (mvp)
-- TODO: keep searched users in "tabs" ?
-- TODO: keep "current" user in path ?


type alias PersistedConfig =
    { username : Maybe String
    , token : Maybe String
    }


type alias Model =
    { gists : WebData (List Gist)
    , display : Display
    , showFiles : Bool
    , username : Maybe String
    , token : Maybe String
    , search : String
    }


authHeader : Maybe String -> Maybe Http.Header
authHeader =
    Maybe.map (Http.header "Authorization" << (\t -> "token " ++ t))


getGists : Maybe String -> String -> Cmd Msg
getGists token username =
    Http.request
        { method = "GET"
        , headers = Maybe.toList <| authHeader token
        , url = "https://api.github.com/users/" ++ username ++ "/gists"
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> GotGists) (D.list gistD)
        , timeout = Nothing
        , tracker = Nothing
        }


init : ( Model, Cmd Msg )
init =
    ( { gists = NotAsked
      , display = Grid
      , showFiles = False
      , username = Nothing
      , token = Nothing
      , search = ""
      }
    , doLoadFromStorage ()
    )



---- -> JSON -> ----


gistD : D.Decoder Gist
gistD =
    D.map8 Gist
        (D.field "id" D.string)
        (D.field "description" <| D.nullable D.string)
        (D.field "html_url" D.string)
        (D.field "files" <| D.dict fileD)
        (D.field "public" D.bool)
        (D.field "created_at" D.string)
        (D.field "updated_at" D.string)
        (D.field "owner" userD)


userD : D.Decoder User
userD =
    D.map4 User
        (D.field "id" D.int)
        (D.field "login" D.string)
        (D.field "html_url" D.string)
        (D.field "avatar_url" D.string)


fileD : D.Decoder File
fileD =
    D.map2 File
        (D.field "filename" D.string)
        (D.field "language" <| D.nullable D.string)


persistedD : D.Decoder PersistedConfig
persistedD =
    D.map2 PersistedConfig
        (D.field "username" <| D.nullable D.string)
        (D.field "token" <| D.nullable D.string)


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
    Result.withDefault { username = Nothing, token = Nothing } << D.decodeValue persistedD



---- UPDATE ----


type Msg
    = GotGists (WebData (List Gist))
    | ChangeDisplay Display
    | ChangeSearch String
    | ChangeToken String
    | ClearToken
    | SearchGists
    | ToggleFiles
    | LoadFromStorage PersistedConfig


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGists gists ->
            ( { model | gists = gists, search = "" }, Cmd.none )

        ChangeDisplay display ->
            ( { model | display = display }, Cmd.none )

        ChangeSearch name ->
            ( { model | search = name }, Cmd.none )

        ChangeToken token ->
            ( { model | token = Just token }
            , saveToStorage <| persistedE { token = Just token, username = model.username }
            )

        ClearToken ->
            ( { model | token = Nothing }
            , saveToStorage <| persistedE { token = Nothing, username = model.username }
            )

        SearchGists ->
            ( { model | gists = Loading, username = Just model.search }
            , Cmd.batch
                [ getGists model.token model.search
                , saveToStorage <| persistedE { token = model.token, username = Just model.search }
                ]
            )

        ToggleFiles ->
            ( { model | showFiles = not model.showFiles }
            , Cmd.none
            )

        LoadFromStorage { username, token } ->
            ( { model
                | username = username
                , token = token
                , gists = Maybe.unwrap NotAsked (always Loading) username
              }
            , Maybe.unwrap Cmd.none (getGists token) username
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ Cx.content ]
        [ Cx.global
        , renderControls model
        , renderTitle model.username
        , renderGists model
        ]


renderTitle : Maybe String -> Html Msg
renderTitle mbUsername =
    case mbUsername of
        Just username ->
            h1 [] [ text <| username ++ " gists" ]

        Nothing ->
            h1 [] [ text "search gists by GitHub username" ]



-- TODO: save token separately from the search
-- TODO: have a "form" field with the WIP values


renderControls : Model -> Html Msg
renderControls { display, showFiles, search, token } =
    div [ Cx.search ]
        [ form [ onSubmit SearchGists ]
            [ input [ Cx.searchInput, onInput ChangeSearch, value search ] []
            , input
                [ Cx.searchInput
                , placeholder "GitHub gist token"
                , onInput ChangeToken
                , value <| Maybe.withDefault "" token
                ]
                []
            , button [ Cx.searchBtn, type_ "submit" ] [ text "Search" ]
            ]
        , renderToggleDisplayBtn display
        , renderToggleFiles showFiles
        , button [ Cx.searchBtn, type_ "button", onClick ClearToken ] [ text "Clear token" ]
        ]


renderToggleDisplayBtn : Display -> Html Msg
renderToggleDisplayBtn display =
    let
        ( msg, label ) =
            case display of
                Grid ->
                    ( ChangeDisplay List, "☷" )

                List ->
                    ( ChangeDisplay Grid, "☰" )
    in
    button [ onClick msg ] [ text label ]


renderToggleFiles : Bool -> Html Msg
renderToggleFiles showFiles =
    let
        label =
            if showFiles then
                "Hide additional files"

            else
                "Show additional files"
    in
    button [ onClick ToggleFiles ] [ text label ]


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
        styles =
            case display of
                Grid ->
                    Cx.gists Cx.gistsGird

                List ->
                    Cx.gists Cx.gistsList
    in
    case gists of
        Success gs ->
            div
                [ styles ]
                (List.map (renderGist display showFiles) gs)

        Failure err ->
            div [] [ renderError err ]

        Loading ->
            div [] [ text "..." ]

        NotAsked ->
            text ""


renderGist : Display -> Bool -> Gist -> Html Msg
renderGist display showFiles { id, html_url, owner, files, public } =
    let
        filesLs =
            Dict.values files

        -- `gistName` is also the name of the first file (unless there's none)
        gistName =
            Maybe.unwrap id .filename <| List.head filesLs

        styles =
            case display of
                Grid ->
                    Cx.gistItem Cx.gistItemGrid

                List ->
                    Cx.gistItem Cx.gistItemList

        fsHtml =
            if showFiles then
                div []
                    << List.map renderFile
                    << Maybe.withDefault []
                <|
                    List.tail filesLs

            else
                text ""

        privateLabel =
            if public then
                text ""

            else
                span [] [ text " *" ]
    in
    div [ styles ]
        [ a
            [ Cx.gistItemLink
            , href html_url
            , target "_blank"
            , title gistName
            ]
            -- TODO: show a lable similar to GitHub's (solve problem with layout)
            [ text <| "/" ++ gistName, privateLabel ]
        , fsHtml
        ]


renderFile : File -> Html Msg
renderFile file =
    div [] [ text file.filename ]



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
