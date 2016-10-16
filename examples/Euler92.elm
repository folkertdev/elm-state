module Euler92 exposing (..)

import Html exposing (text, Html)
import State exposing (..)
import Dict exposing (Dict)
import Array exposing (Array)


digits : Int -> List Int
digits value =
    if value == 0 then
        [ 0 ]
    else
        value % 10 :: digits (value // 10)


square : Int -> Int
square x =
    x * x


step : Int -> Int
step =
    List.foldl ((+) << square) 0 << digits


terminator : Int -> State (Dict Int Int) Int
terminator n =
    if n == 1 || n == 89 then
        state n
    else
        let
            updateWithValue : Int -> State (Dict Int Int) Int
            updateWithValue value =
                modify (Dict.insert n value)
                    |> State.map (\_ -> value)

            updateIfNeeded : Dict Int Int -> State (Dict Int Int) Int
            updateIfNeeded dict =
                case Dict.get n dict of
                    Just v ->
                        state v

                    Nothing ->
                        terminator (step n)
                            |> andThen updateWithValue
        in
            get
                |> andThen updateIfNeeded


terminators : List Int -> State (Dict Int Int) (List Int)
terminators =
    State.traverse terminator


solution : Int -> Int
solution n =
    let
        {- calculates the value less than n with highest step value -}
        upperLimit : Int
        upperLimit =
            logBase 10 (toFloat n)
                |> ceiling
                |> (\v -> (10 ^ v) - 1)

        {- The cache stores the numbers
           1 up to upperLimit. This means that (step n) for n <= upperLimit is always in
           the cache.
        -}
        cache : Dict Int Int
        cache =
            finalState Dict.empty (terminators [1..(step upperLimit)])
                |> Dict.insert 1 1
                |> Dict.insert 89 89

        {- many functions in a single fold
           Elm does not have fusion (yet)
        -}
        operation : Int -> Int -> Int
        operation =
            let
                filterLength =
                    (\v accum ->
                        if v == Just 89 then
                            accum + 1
                        else
                            accum
                    )
            in
                filterLength << (\e -> Dict.get e cache) << step
    in
        Array.initialize n (\v -> v + 1)
            |> Array.foldr operation 0


main =
    solution 99999
        |> toString
        |> text
