module SieveOfEratosthenes exposing (..)

import State exposing (state, andThen, State)
import Array exposing (Array)
import Html exposing (text)


type alias Sieve =
    Array (Maybe Int)


type alias Config =
    { username : String }


range : Int -> Int -> Int -> List Int
range start stop step =
    List.range 0 ((stop - start) // step)
        |> List.map (\v -> v * step + start)


cycle : Int -> State Sieve (Maybe Int)
cycle n =
    let
        mark : Int -> State Sieve ()
        mark n =
            State.modify (Array.set n Nothing)

        -- generate multiples of n in the range [n*n..lengthOfArray]
        multiplesToMark length n =
            range (n * n) length n

        getArrayLength =
            State.map Array.length State.get

        markMultiples length =
            -- traverse shares the Array Int beteween invocations of mark.
            State.traverse mark (multiplesToMark length n)

        setNextIndex _ =
            State.map (toNextIndex n) State.get
    in
        getArrayLength
            |> andThen markMultiples
            |> andThen setNextIndex


recurse : Int -> (Int -> State Sieve (Maybe Int)) -> State Sieve (Maybe Int)
recurse initial advance =
    let
        advanceIfPossible : Maybe Int -> State Sieve (Maybe Int)
        advanceIfPossible mindex =
            case mindex of
                Nothing ->
                    state Nothing

                Just index ->
                    recurse index advance
    in
        advance initial |> andThen advanceIfPossible


toNextIndex : Int -> Sieve -> Maybe Int
toNextIndex currentIndex sieve =
    let
        predicate e =
            case e of
                Just v ->
                    v > currentIndex

                Nothing ->
                    False
    in
        Array.filter predicate sieve
            |> Array.get 0
            |> (\m -> m |> Maybe.andThen identity)


primesUpTo : Int -> Array Int
primesUpTo n =
    -- up to, not including
    let
        initialState =
            Array.initialize n Just
                |> Array.set 0 Nothing
                |> Array.set 1 Nothing
    in
        recurse 2 cycle
            |> State.finalState initialState
            |> Array.foldl
                (\elem accum ->
                    case elem of
                        Nothing ->
                            accum

                        Just v ->
                            Array.push v accum
                )
                Array.empty


main =
    primesUpTo 100
        |> toString
        |> text
