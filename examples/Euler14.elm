module Euler14 exposing (..)

import State exposing (State)
import Cache
import Dict exposing (Dict)
import Html exposing (text)


step : Int -> Int
step n =
    if n % 2 == 0 then
        n // 2
    else
        3 * n + 1


increment : Int -> State (Dict Int (List Int)) (List Int)
increment n =
    State.map (\rest -> n :: rest) (generate (step n))


generate : Int -> State (Dict Int (List Int)) (List Int)
generate n =
    Cache.generateIfMissing increment n


generateMany : List Int -> State (Dict Int (List Int)) (List (List Int))
generateMany =
    State.traverse generate


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


{-| Picking a much higher upper bound won't work,
because of the recursion limit. The problem is the
size of the Dict, not State.
-}
upper =
    2


main =
    generateMany [1..upper]
        |> State.finalState (Dict.fromList [ ( 1, [ 1 ] ) ])
        |> Dict.filter (\key _ -> key <= upper)
        --     |> Dict.foldl predicate Nothing
        |>
            toString
        |> text
