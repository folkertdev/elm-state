module Euler14 exposing (..)

import State exposing (State, andThen)
import Dict exposing (Dict)
import Html exposing (text)
import List.Extra as List


step n =
    if n % 2 == 0 then
        n // 2
    else
        3 * n + 1


increment n =
    step n
        |> generate
        |> State.map (\rest -> n :: rest)


generate n =
    generateIfMissing increment n


generates =
    State.traverse generate


generateIfMissing : (comparable -> State (Dict comparable value) value) -> comparable -> State (Dict comparable value) value
generateIfMissing generator key =
    let
        modifyWith : (a -> s -> s) -> a -> State s a
        modifyWith f value =
            State.modify (f value)
                |> State.map (\_ -> value)

        updateIfNeeded cache =
            case Dict.get key cache of
                Just v ->
                    State.state v

                Nothing ->
                    let
                        result =
                            generator key
                    in
                        generator key
                            |> andThen (modifyWith (Dict.insert key))
    in
        State.get
            |> andThen updateIfNeeded


upper =
    500000


folder upper key value accum =
    if key <= upper then
        let
            size =
                List.length value
        in
            case accum of
                Nothing ->
                    Just ( key, size )

                Just ( keyAccum, valueAccum ) ->
                    if size > valueAccum then
                        Just ( key, size )
                    else
                        accum
    else
        accum


largest upper =
    generates (List.range 1 upper)
        |> State.finalState (Dict.fromList [ ( 1, [ 1 ] ) ])
        |> Dict.foldr (folder upper) Nothing


main =
    largest upper
        |> toString
        |> text


alternative a b =
    case a of
        Just v ->
            Just v

        Nothing ->
            b


predicate key value accum =
    let
        size =
            List.length value
    in
        alternative (Just size) (Maybe.map (max size) accum)
