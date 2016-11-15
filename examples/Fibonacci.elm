module Fibonacci exposing (..)

import State exposing (state, andThen, State)
import Dict exposing (Dict)
import Html exposing (Html, text)


fib : Int -> Int
fib n =
    let
        initialState =
            Dict.fromList [ ( 0, 1 ), ( 1, 1 ) ]
    in
        State.finalValue initialState (fibHelper n)


fibHelper : Int -> State (Dict Int Int) Int
fibHelper n =
    let
        -- takes n as an argument to preserver laziness
        calculateStatefullFib : Int -> State (Dict Int Int) Int
        calculateStatefullFib n =
            State.map2 (+) (fibHelper (n - 1)) (fibHelper (n - 2))

        addNewValue : Int -> State (Dict Int Int) Int
        addNewValue solution =
            State.modify (Dict.insert n solution)
                |> State.map (\_ -> solution)

        modifyWhenNeeded : Dict Int Int -> State (Dict Int Int) Int
        modifyWhenNeeded cache =
            case Dict.get n cache of
                Just cachedSolution ->
                    state cachedSolution

                Nothing ->
                    calculateStatefullFib n
                        |> andThen addNewValue
    in
        State.get
            |> andThen modifyWhenNeeded


fibs : List Int -> List Int
fibs =
    let
        initialState =
            Dict.fromList [ ( 0, 1 ), ( 1, 1 ) ]
    in
        State.finalValue initialState << fibsHelper


fibsHelper : List Int -> State (Dict Int Int) (List Int)
fibsHelper =
    -- State.mapState makes sure to reuse the cache between calls to fibHelper.
    State.traverse fibHelper


main =
    List.range 0 9
        |> fibs
        |> toString
        |> text
