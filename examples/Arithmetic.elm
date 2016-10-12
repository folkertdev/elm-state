module Arithmetic exposing (..)

import State exposing (State, andThen, modify)


type Expr
    = Plus
    | Val Int


type alias Stack a b =
    State (List a) b


type alias StackUpdate =
    Stack Expr () -> Stack Expr ()


emptyStack : Stack a ()
emptyStack =
    State.get
        |> andJust (State.modify identity)


example =
    emptyStack
        |> pushVal 2
        |> pushVal 3
        |> pushVal 4
        |> plus
        |> plus


evaluate : Stack a b -> Maybe a
evaluate =
    List.head << State.finalState []


andJust : State s b -> State s a -> State s b
andJust expr =
    State.andThen (\_ -> expr)


plus : StackUpdate
plus =
    let
        helper stack =
            case stack of
                (Val x) :: (Val y) :: rest ->
                    -- the operator evaluates the result if possible
                    Val (x + y) :: rest

                _ ->
                    Debug.crash "invalid arguments for (+)"
    in
        modify helper
            |> andJust


pushVal : Int -> StackUpdate
pushVal x =
    modify (\stack -> Val x :: stack)
        |> andJust
