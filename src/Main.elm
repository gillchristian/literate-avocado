module Main exposing (Model, Msg(..), User, init, main, update, view)

import Browser
import Cx
import Dict as Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (href, src, target, title, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http exposing (Error(..))
import Json.Decode as D
import List as List
import Maybe as Maybe
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


type alias Model =
    { gists : WebData (List Gist)
    , display : Display
    , showFiles : Bool
    , username : String -- Maybe String
    , search : String
    }


getGists : String -> Cmd Msg
getGists username =
    Http.get
        { url = "https://api.github.com/users/" ++ username ++ "/gists"
        , expect = Http.expectJson (RemoteData.fromResult >> GotGists) (D.list gistDecoder)
        }


init : ( Model, Cmd Msg )
init =
    let
        username =
            "gillchristian"
    in
    ( { gists = Loading
      , display = Grid
      , showFiles = False
      , username = username
      , search = ""
      }
    , getGists username
    )



---- -> JSON -> ----


gistDecoder : D.Decoder Gist
gistDecoder =
    D.map8 Gist
        (D.field "id" D.string)
        (D.field "description" <| D.nullable D.string)
        (D.field "html_url" D.string)
        (D.field "files" <| D.dict fileDecoder)
        (D.field "public" D.bool)
        (D.field "created_at" D.string)
        (D.field "updated_at" D.string)
        (D.field "owner" userDecoder)


userDecoder : D.Decoder User
userDecoder =
    D.map4 User
        (D.field "id" D.int)
        (D.field "login" D.string)
        (D.field "html_url" D.string)
        (D.field "avatar_url" D.string)


fileDecoder : D.Decoder File
fileDecoder =
    D.map2 File
        (D.field "filename" D.string)
        (D.field "language" <| D.nullable D.string)



---- UPDATE ----


type Msg
    = GotGists (WebData (List Gist))
    | ChangeDisplay Display
    | ChangeSearch String
    | SearchGists
    | ToggleFiles


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGists gists ->
            ( { model | gists = gists, search = "" }, Cmd.none )

        ChangeDisplay display ->
            ( { model | display = display }, Cmd.none )

        ChangeSearch name ->
            ( { model | search = name }, Cmd.none )

        SearchGists ->
            ( { model | gists = Loading, username = model.search }
            , getGists model.search
            )

        ToggleFiles ->
            ( { model | showFiles = not model.showFiles }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ Cx.content ]
        [ Cx.global
        , renderControls model
        , h1 [] [ text <| model.username ++ " gists" ]
        , renderGists model
        ]


renderControls : Model -> Html Msg
renderControls { display, showFiles, search } =
    div [ Cx.search ]
        [ form [ onSubmit SearchGists ]
            [ input [ Cx.searchInput, onInput ChangeSearch, value search ] []
            , button [ Cx.searchBtn, type_ "submit" ] [ text "Search" ]
            ]
        , renderToggleDisplayBtn display
        , renderToggleFiles showFiles
        ]


renderToggleDisplayBtn : Display -> Html Msg
renderToggleDisplayBtn display =
    button
        [ onClick <|
            case display of
                Grid ->
                    ChangeDisplay List

                List ->
                    ChangeDisplay Grid
        ]
        [ text <|
            case display of
                Grid ->
                    "☷"

                List ->
                    "☰"
        ]


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
            text str

        Timeout ->
            div []
                [ p [] [ text "Request timed out." ]
                ]

        NetworkError ->
            div []
                [ p [] [ text "Looks like you are offline." ]
                , p [] [ text "Check your connection and try again." ]
                ]

        BadStatus code ->
            text <| "Status:" ++ String.fromInt code

        BadBody msg ->
            text <| "BadBody: " ++ msg


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

        _ ->
            -- TODO: spinner / animation ?
            div [] [ text "..." ]


renderGist : Display -> Bool -> Gist -> Html Msg
renderGist display showFiles { id, html_url, owner, files } =
    let
        filesLs =
            Dict.toList files

        -- `gistName` is also the name of the first file (unless there's none)
        gistName =
            Maybe.withDefault id <|
                Maybe.map (.filename << second) <|
                    List.head filesLs

        styles =
            case display of
                Grid ->
                    Cx.gistItem Cx.gistItemGrid

                List ->
                    Cx.gistItem Cx.gistItemList

        fsHtml =
            if showFiles then
                List.map renderFile <| Maybe.withDefault [] <| List.tail filesLs

            else
                []

        fs =
            div [] fsHtml
    in
    div [ styles ]
        [ a
            [ Cx.gistItemLink
            , href html_url
            , target "_blank"
            , title gistName
            ]
            [ text <| "/" ++ gistName ]
        , fs
        ]


renderFile : ( String, File ) -> Html Msg
renderFile ( name, _ ) =
    -- TODO: add styles
    div [] [ text name ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
