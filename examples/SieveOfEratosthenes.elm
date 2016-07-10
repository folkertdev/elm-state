module SieveOfEratosthenes exposing (..)

import State exposing (state, andThen, State)
import Array exposing (Array)
import Html exposing (text)


range : Int -> Int -> Int -> List Int
range start stop step =
    [0..((stop - start) // step)]
        |> List.map (\v -> v * step + start)


cycle : Int -> State (Array Bool) (Maybe Int)
cycle n =
    let
        mark : Int -> State (Array Bool) ()
        mark n =
            State.modify (Array.set n False)

        -- generate multiples of n in the range [n*n..lengthOfArray]
        multiplesToMark length n =
            range (n * n) length n

        getArrayLength =
            State.map Array.length State.get

        markMultiples length =
            -- mapState shares the Array Int beteween invocations of mark.
            State.mapState mark (multiplesToMark length n)

        setNextIndex _ =
            State.map (toNextIndex n) State.get
    in
        getArrayLength `andThen` markMultiples `andThen` setNextIndex


recurse : Int -> (Int -> State (Array Bool) (Maybe Int)) -> State (Array Bool) (Maybe Int)
recurse initial advance =
    let
        advanceIfPossible : Maybe Int -> State (Array Bool) (Maybe Int)
        advanceIfPossible mindex =
            Maybe.map (\index -> recurse index advance) mindex
                |> Maybe.withDefault (state Nothing)
    in
        advance initial `andThen` advanceIfPossible


toNextIndex : Int -> Array Bool -> Maybe Int
toNextIndex currentIndex array =
    Array.indexedMap (,) array
        |> Array.filter (\( index, value ) -> index > currentIndex && value == True)
        |> Array.get 0
        |> Maybe.map fst


primesUpTo : Int -> Array Int
primesUpTo n =
    -- up to, not including
    let
        initialState =
            Array.repeat n True
                |> Array.set 0 False
                |> Array.set 1 False
    in
        recurse 2 cycle
            |> State.finalState initialState
            |> Array.indexedMap (,)
            |> Array.filter (\( i, v ) -> v == True)
            |> Array.map fst


main =
    primesUpTo 100
        |> toString
        |> text
