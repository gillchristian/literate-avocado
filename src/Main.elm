module Main exposing (Model, Msg(..), User, init, main, update, view)

import Browser
import Dict as Dict exposing (Dict)
import Html exposing (Html, a, div, h1, img, span, text)
import Html.Attributes exposing (href, src)
import Http exposing (Error(..))
import Json.Decode as D
import List as List
import RemoteData exposing (RemoteData(..), WebData)
import String as String



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
    , description : String
    , html_url : String
    , files : Dict String File
    , public : Bool
    , created_at : String
    , updated_at : String
    , owner : User
    }


type alias Model =
    { gists : WebData (List Gist) }


getGists : Cmd Msg
getGists =
    Http.get
        { url = "https://api.github.com/users/gillchristian/gists"
        , expect = Http.expectJson (RemoteData.fromResult >> GotText) (D.list gistDecoder)
        }


init : ( Model, Cmd Msg )
init =
    ( { gists = Loading }, getGists )



---- -> JSON -> ----


gistDecoder : D.Decoder Gist
gistDecoder =
    D.map8 Gist
        (D.field "id" D.string)
        (D.field "description" D.string)
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
    = GotText (WebData (List Gist))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText res ->
            ( { gists = res }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { gists } =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , case gists of
            NotAsked ->
                div [] [ text "NotAsked" ]

            Loading ->
                div [] [ text "Loading" ]

            Success gs ->
                div [] (List.map gistView gs)

            Failure err ->
                div [] [ errorView err ]
        ]


errorView : Http.Error -> Html Msg
errorView err =
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


gistView : Gist -> Html Msg
gistView { id, html_url, owner, files } =
    div []
        [ div []
            [ a
                [ href <| "https://gist.github.com/" ++ owner.login ]
                [ text owner.login ]
            , span [] [ text " / " ]
            , a [ href html_url ] [ text id ]
            ]
        , div [] <| List.map fileView <| Dict.toList files
        ]


fileView : ( String, File ) -> Html Msg
fileView ( name, _ ) =
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
