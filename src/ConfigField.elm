module ConfigField exposing
    ( ConfigField(..)
    , fromMaybe
    , toMaybe
    , withDefault
    )


type ConfigField a
    = Empty
    | Editing a
    | Saved a


toMaybe : ConfigField a -> Maybe a
toMaybe cf =
    case cf of
        Saved a ->
            Just a

        _ ->
            Nothing


fromMaybe : Maybe a -> ConfigField a
fromMaybe cf =
    case cf of
        Just a ->
            Saved a

        _ ->
            Empty


withDefault : a -> ConfigField a -> a
withDefault def cf =
    case cf of
        Saved a ->
            a

        _ ->
            def
