module Main exposing (Model, Msg(..), User, init, main, update, view)

import Browser
import Dict as Dict exposing (Dict)
import Html exposing (Html, a, button, div, h1, input, span, text)
import Html.Attributes exposing (class, href, src, target, title, value)
import Html.Events exposing (onClick, onInput)
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


type alias Model =
    { gists : WebData (List Gist)
    , display : Display
    , username : String
    , search : String
    }


getGists : String -> Cmd Msg
getGists username =
    Http.get
        { url = "https://api.github.com/users/" ++ username ++ "/gists"
        , expect = Http.expectJson (RemoteData.fromResult >> GotGists) (D.list gistDecoder)
        }



-- TODO: load user from localStorage


init : ( Model, Cmd Msg )
init =
    let
        username =
            "gillchristian"
    in
    ( { gists = Loading
      , display = List
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



---- VIEW ----


view : Model -> Html Msg
view { display, gists, username, search } =
    div [ class "content" ]
        [ h1 [] [ text <| username ++ " gists" ]
        , renderToggleDisplayBtn display
        , div []
            [ input [ onInput ChangeSearch, value search ] []
            , button [ onClick SearchGists ] [ text "Search" ]
            ]
        , renderGists display gists
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


renderError : Http.Error -> Html Msg
renderError err =
    case err of
        BadUrl str ->
            text str

        Timeout ->
            text "Timeout"

        NetworkError ->
            text "NetworkError"

        BadStatus code ->
            text <| "Status:" ++ String.fromInt code

        BadBody msg ->
            text <| "BadBody: " ++ msg


renderGists : Display -> WebData (List Gist) -> Html Msg
renderGists display gists =
    let
        cx =
            case display of
                Grid ->
                    "gists gists-grid"

                List ->
                    "gists gists-list"
    in
    case gists of
        Success gs ->
            div
                [ class cx ]
                (List.map (renderGist display) gs)

        Failure err ->
            div [] [ renderError err ]

        _ ->
            div [] [ text "..." ]


renderGist : Display -> Gist -> Html Msg
renderGist display { id, html_url, owner, files } =
    let
        filesLs =
            Dict.toList files

        gistName =
            Maybe.withDefault id <|
                Maybe.map (.filename << second) <|
                    List.head filesLs

        cx =
            case display of
                Grid ->
                    "gist-item gist-item-grid"

                List ->
                    "gist-item gist-item-list"
    in
    div [ class <| cx ]
        [ a
            [ href html_url, target "_blank", title gistName ]
            [ text <| "/" ++ gistName ]

        -- TODO: render files if there are more than one
        -- TODO: make it toggleable
        -- , div [] <| List.map renderFile filesLs
        ]


renderFile : ( String, File ) -> Html Msg
renderFile ( name, _ ) =
    div [] [ text name ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
