## TODO

Checkout [arturopala/elm-monocle/2.2.0](https://package.elm-lang.org/packages/arturopala/elm-monocle/2.2.0/)

## Code

```elm
import Monocle.Compose as Compose
import Monocle.Lens as Lens exposing (Lens)
import Monocle.Optional as Optional exposing (Optional)
import Monocle.Prism as Prism exposing (Prism)

gistsLens : Lens Model (WebData Gists)
gistsLens =
    let
        set gists model =
            { model | gists = gists }
    in
    Lens .gists set


itemsLens : Lens Gists (List Gist)
itemsLens =
    let
        set items gists =
            { gists | items = items }
    in
    Lens .items set


tagsLens : Lens Gists (Set String)
tagsLens =
    let
        set tags gists =
            { gists | tags = tags }
    in
    Lens .tags set


modelToGistsItems : Optional Model (List Gist)
modelToGistsItems =
    gistsLens
        |> Compose.lensWithPrism RemoteData.prism
        |> Compose.optionalWithLens itemsLens


modelToGistsTags : Optional Model (Set String)
modelToGistsTags =
    gistsLens
        |> Compose.lensWithPrism RemoteData.prism
        |> Compose.optionalWithLens tagsLens
```
