module Fibonacci exposing (..)

import State exposing (state, andThen, State)
import Dict exposing (Dict)
import Html exposing (Html, text)
import Trampoline exposing (..)


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
    -- fibs [0..9]
    replicateM' 10 (State.state [ 2 ])
        |> State.run 3
        |> toString
        |> text


tailRec : (a -> State s (Result a b)) -> a -> State s b
tailRec f x =
    let
        go ( x, s ) =
            case State.run s (f x) of
                ( Err x, s ) ->
                    go ( x, s )

                ( Ok x, s ) ->
                    ( x, s )
    in
        State.advance (\s -> go ( x, s ))


replicateM : Int -> State s a -> State s (List a)
replicateM n state =
    let
        go ( n, xs ) =
            if n < 1 then
                State.state (Ok xs)
            else
                State.map (\x -> Err ( n - 1, x :: xs )) state
    in
        tailRec go ( n, [] )


replicateM' : Int -> State s a -> State s (List a)
replicateM' n s =
    let
        go ( n, xs ) =
            if n <= 0 then
                done xs
            else
                jump (\_ -> go ( n - 1, State.map2 (::) s xs ))
    in
        go ( n, state [] )
            |> evaluate
