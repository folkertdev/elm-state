module Cache exposing (..)

import State exposing (..)
import Dict exposing (Dict)


{-| Generates the value for the given key only when it's not already in the cache.

To generate the missing value, the supplied generator function is used. The
generated value is accessible, for instance with `State.map` or `State.finalValue`.
-}
generateIfMissing : (comparable -> State (Dict comparable a) a) -> comparable -> State (Dict comparable a) a
generateIfMissing generator key =
    let
        modifyWith : (a -> s -> s) -> a -> State s a
        modifyWith f value =
            State.modify (f value)
                |> State.map (\_ -> value)

        helper cache =
            case Dict.get key cache of
                Just cachedValue ->
                    State.state cachedValue

                Nothing ->
                    generator key
                        |> andThen (modifyWith (Dict.insert key))
    in
        State.get
            |> andThen helper
