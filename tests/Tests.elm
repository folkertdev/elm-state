module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import State exposing (State, andThen, state)


evaluate a b =
    Expect.equal (State.run () a) (State.run () b)


all : Test
all =
    describe "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "Join = AndThen identity" <|
                \() ->
                    let
                        a =
                            state (state 2)
                                |> andThen identity

                        b =
                            state (state 2)
                                |> State.join
                    in
                        evaluate a b
            , test "Join = AndThen identity" <|
                \() ->
                    let
                        a =
                            State.map (\x -> x + 2) (state 2)

                        b =
                            state 2
                                |> andThen (\x -> state (x + 2))
                    in
                        evaluate a b
            , test "filterM documentation example" <|
                \() ->
                    let
                        like : String -> String
                        like subject =
                            "I like " ++ subject ++ "s"

                        rodents =
                            [ "hamster", "rabbit", "guinea pig" ]

                        result =
                            State.filterM (State.embed << List.member) rodents
                                |> State.map (List.map like)
                                |> State.map (String.join " and ")
                                |> State.run [ "cat", "dog", "hamster" ]
                    in
                        Expect.equal ( "I like hamsters", [ "cat", "dog", "hamster" ] ) result
            , test "get documentation example" <|
                \() ->
                    State.map2 (+) State.get State.get
                        |> State.run 42
                        |> Expect.equal ( 84, 42 )
            , test "set documentation example" <|
                \() ->
                    State.put 5
                        |> State.run 3
                        |> Expect.equal ( (), 5 )
            , test "modify documentation example" <|
                \() ->
                    State.modify (\v -> v + 1)
                        |> State.map (\_ -> "finished")
                        |> State.run 42
                        |> Expect.equal ( "finished", 43 )
            ]
        , describe "Fuzz test examples, using randomly generated input"
            [ fuzz (list int) "Lists always have positive length" <|
                \aList ->
                    List.length aList |> Expect.atLeast 0
            ]
        , describe "Monad Laws"
            [ fuzz int "Left identity" <|
                let
                    f x =
                        state (x + x)
                in
                    \value -> evaluate (state value |> andThen f) (f value)
            , fuzz int "Right identity" <|
                let
                    f x =
                        state (x + x)
                in
                    \value -> evaluate (state value |> andThen state) (state value)
            , fuzz int "Associativity" <|
                \value ->
                    let
                        f x =
                            state (x + x)

                        g x =
                            state (x * x)

                        m =
                            state 2
                    in
                        evaluate (andThen g (andThen f m)) (andThen (\x -> f x |> andThen g) m)
            ]
        , describe "traverse"
            [ test "expect traverse leaves the list the same" <|
                \() ->
                    let
                        list =
                            [ 1, 2, 3 ]
                    in
                        list
                            |> State.traverse State.state
                            |> State.finalValue ()
                            |> Expect.equal list
            , test "foldrM doesn't blow the stack" <|
                \() ->
                    List.range 0 100000
                        |> State.foldrM (\a b -> state (a + b)) 0
                        |> State.finalValue ()
                        |> Expect.equal (List.foldr (+) 0 <| List.range 0 100000)
            , test "filterM doesn't blow the stack" <|
                \() ->
                    List.range 0 100000
                        |> State.filterM (\x -> state (x > -1))
                        |> State.finalValue ()
                        |> Expect.equal (List.filter (\v -> v > -1) <| List.range 0 100000)
            ]
        ]
