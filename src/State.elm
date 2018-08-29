module State exposing
    ( State(..), state, embed, advance
    , map, map2, map3
    , andMap, andThen, join
    , get, put, modify
    , run, finalValue, finalState
    , traverse, combine, filterM, foldlM, foldrM
    , tailRec, tailRecM, Step(..)
    )

{-| This library provides ways to compose functions of the type
`s -> (a, s)`. This composition threads state through a computation

From time to time, you'll see a pattern like this in your code

    ( newValue, newState ) =
        f state

    ( newerValue, newerState ) =
        g newValue newState

    ( newererValue, newererState ) =
        h newerValue newerState

This pattern is ugly and error-prone (because of typo's, for instance).
It can be abstracted by creating a function that composes `f` and `g` (
the output of `f` is the input to `g`).

    f : s -> ( a, s )

    g : a -> s -> ( a, s )

This library implements this composition and provides a bunch of helper functions for
working with State. For a more in-depth explanation of how the implementation works,
see the [derivation](https://github.com/folkertdev/elm-state#derivation). For more detailed, higher
level documentation, please see the [readme](https://github.com/folkertdev/elm-state) and the [examples](https://github.com/folkertdev/elm-state/tree/master/examples)

#Type and Constructors

@docs State, state, embed, advance

#Mapping

@docs map, map2, map3

#Chaining

@docs andMap, andThen, join

#Changing State

@docs get, put, modify

#Running State

@docs run, finalValue, finalState

#Generalized list functions

@docs traverse, combine, filterM, foldlM, foldrM

#Safe recursion

The archetypal Haskell implementation for State will overflow the stack in strict languages like Elm.
We use the fact that elm performs tail-call eliminiation to make sure the generalized list functions don't overflow the stack.

To allow for full flexibility, the stack-safety primitives are exposed. Look at the source for examples.

The implementation of these functions is heavily based on the
[purescript MonadRec implementation](https://github.com/purescript/purescript-tailrec/blob/master/src/Control/Monad/Rec/Class.purs)

@docs tailRec, tailRecM, Step

#Notes for the Haskellers/curious

The `State` type of this package is the `State Monad`. This wording is a little weird, it'd be better to say that
`State` is a `Monad`.

Monad is a concept from a branch of mathematics called category theory. In short, it is a type on which
`andThen` is defined (examples in core are Random, Maybe, Result and Decoder). Many useful types are monads,
and therefore being familiar with the concept can be very helpful in functional programming.

Monads are also called 'computation builders': They allow for an elegant way of chaining computations with `andThen`
(see the [README](https://github.com/folkertdev/elm-state#structuring-computation-with-andthen)).
Elm wants to be a simple, easy to learn language, and therefore monads aren't really talked about (yet). I've tried to limit the jargon in the documentation to a minimum.
If anything in the docs here or in the repository is still unclear, please open an issue [on the repo](https://github.com/folkertdev/elm-state/issues).

-}

-- Type and Constructors


{-| Type that represents state.

Note that `State` wraps a function, not a concrete value.

-}
type State state value
    = State (state -> ( value, state ))


{-| Create a new State from a value of any type.
-}
state : value -> State state value
state value =
    State (\s -> ( value, s ))


{-| Embed a function into State. The function is applied to the state, the result
will become the value.

It is implemented as:

    embed : (a -> b) -> State a b
    embed f =
        State (\s -> ( f s, s ))

This function can be extended as follows:

    embed2 : (a -> b -> c) -> a -> State b c
    embed2 f arg1 =
        embed (f arg1)

-}
embed : (s -> a) -> State s a
embed f =
    State (\s -> ( f s, s ))


{-| Wrap a function as a State. Remember that `State` is just a wrapper around
a function of type `s -> ( a, s )`.
-}
advance : (s -> ( a, s )) -> State s a
advance f =
    State f



-- Mapping


{-| Apply a function to the value that the state holds
-}
map : (a -> b) -> State s a -> State s b
map f (State step) =
    State <|
        \currentState ->
            let
                ( value, newState ) =
                    step currentState
            in
            ( f value, newState )


{-| Apply a function to the value of two states. The newest state will be kept
-}
map2 : (a -> b -> c) -> State s a -> State s b -> State s c
map2 f (State step1) (State step2) =
    State <|
        \currentState ->
            let
                ( value1, newState ) =
                    step1 currentState

                ( value2, newerState ) =
                    step2 newState
            in
            ( f value1 value2, newerState )


{-| Apply a function to the value of three states. The newest state will be kept

The definition of map3 is in terms of andMap, which can be used to create
map4, map5 ect.

    map3 :
        (a -> b -> c -> d)
        -> State s a
        -> State s b
        -> State s c
        -> State s d
    map3 f step1 step2 step3 =
        map f step1
            |> andMap step2
            |> andMap step3

-}
map3 : (a -> b -> c -> d) -> State s a -> State s b -> State s c -> State s d
map3 f step1 step2 step3 =
    map f step1
        |> andMap step2
        |> andMap step3



-- Chaining


{-| Apply a function wrapped in a state to a value wrapped in a state.
This is very useful for applying stateful arguments one by one.

The use of `andMap` can be substituted by using mapN. The following
expressions are equivalent.

    map f arg1 |> andMap arg2 == State.map2 f arg1 arg2

In general, using the `mapN` functions is preferable. The `mapN` functions can
be defined up to an arbitrary `n` using `andMap`.

    State.mapN f arg1 arg2 ... argN
        == State.map f arg1
                |> andMap arg2
                ...
                |> andMap argN

-}
andMap : State s a -> State s (a -> b) -> State s b
andMap =
    \b a -> map2 (<|) a b


{-| Chain two operations with state.

The [readme](https://github.com/folkertdev/elm-state) has a section on [structuring computation
with `andThen`](https://github.com/folkertdev/elm-state#structuring-computation-with-andthen).

-}
andThen : (a -> State s b) -> State s a -> State s b
andThen f (State h) =
    State <|
        \s ->
            let
                ( a, newState ) =
                    h s

                (State g) =
                    f a
            in
            g newState


{-| Discard a level of state.
-}
join : State s (State s a) -> State s a
join (State f) =
    -- andThen identity (State f)
    State <|
        \s ->
            let
                ( State g, newState ) =
                    f s
            in
            g newState



-- Changing the state


{-| Get the current state. Typically the state is
modified somehow and then put back with put.
-}
get : State s s
get =
    State (\s -> ( s, s ))


{-| Replace the current state with a new one.
-}
put : s -> State s ()
put x =
    State (\_ -> ( (), x ))


{-| Modify the state. This is a combination of set and put

An example using `State.get` and `State.modify`:

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

                updateIfNeeded :
                    Dict Int Int
                    -> State (Dict Int Int) Int
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

-}
modify : (s -> s) -> State s ()
modify f =
    State (\s -> ( (), f s ))


{-| Thread the state through a computation,
and return both the final state and the computed value

Note for Haskellers: the argument order is swapped. This is more
natural in elm because code is often structured left to right using `(|>)`.

-}
run : s -> State s a -> ( a, s )
run initialState (State s) =
    s initialState


{-| Thread the state through a computation,
and return only the computed value


    fibs : List Int -> List Int
    fibs =
        let
            initialState =
                Dict.fromList [ ( 0, 1 ), ( 1, 1 ) ]
        in
        State.finalValue initialState << fibsHelper


    -- fibsHelper : List Int -> State (Dict Int Int) (List Int)

See [Fibonacci.elm](https://github.com/folkertdev/elm-state/blob/master/examples/Fibonacci.elm) for the full example.

-}
finalValue : s -> State s a -> a
finalValue initialState =
    Tuple.first << run initialState


{-| Thread the state through a computation,
and return only the final state

    primesUpTo : Int -> Array Int
    primesUpTo n =
        let
            initialState =
                Array.repeat n True
                    |> Array.set 0 False
                    |> Array.set 1 False
        in
        recurse 2 cycle
            |> State.finalState initialState
            |> Array.indexedMap (\a b -> ( a, b ))
            |> Array.filter (\( i, v ) -> v == True)
            |> Array.map fst

See [SieveOfErastosthenes.elm](https://github.com/folkertdev/elm-state/blob/master/examples/SieveOfEratosthenes.elm) for the full example.

-}
finalState : s -> State s a -> s
finalState initialState =
    Tuple.second << run initialState



-- Generalized list functions


{-| Generalize `List.map` to work with `State`.

When you have a function the works on a single element,

    mark : Int -> State (Array Bool) ()
    mark index =
        State.modify (Array.set index False)

traverse can be used to let it work on a list of elements,
taking care of threading the state through.

    markMany : List Int -> State (Array Bool) (List ())
    markMany =
        State.traverse mark

This function is also called `mapM`.

-}
traverse : (a -> State s b) -> List a -> State s (List b)
traverse f =
    map List.reverse << foldlM (\accum elem -> map2 (::) (f elem) (state accum)) []


{-| Combine a list of `State`s into one by composition.
The resulting value is a list of the results of subcomputations.
-}
combine : List (State s a) -> State s (List a)
combine =
    traverse identity


{-| Generalize `List.filter` to work on `State`. Composes only the states that satisfy the predicate.

    -- keep only items that occur at least once
    [ 1, 2, 3, 4, 4, 5, 5, 1 ]
        |> State.filter (\element -> State.advance (\cache -> (List.member element cache, element :: cache)))
        |> State.run []
        --> ([4,5,1], [1,5,5,4,4,3,2,1])

-}
filterM : (a -> State s Bool) -> List a -> State s (List a)
filterM predicate =
    let
        folder elem accum =
            let
                keepIfTrue verdict =
                    if verdict then
                        elem :: accum

                    else
                        accum
            in
            predicate elem
                |> map keepIfTrue
    in
    map List.reverse << foldlM (\b a -> folder a b) []


{-| Compose a list of updated states into one from the left. Also called `foldM`.
-}
foldlM : (b -> a -> State s b) -> b -> List a -> State s b
foldlM f =
    let
        step accum elements =
            case elements of
                [] ->
                    state (Done accum)

                x :: xs ->
                    f accum x
                        |> map (\a_ -> Loop ( a_, xs ))
    in
    tailRecM2 step


{-| Compose a list of updated states into one from the right
-}
foldrM : (a -> b -> State s b) -> b -> List a -> State s b
foldrM f initialValue xs =
    foldlM (\b a -> f a b) initialValue (List.reverse xs)


{-| Perform an action n times, gathering the results
-}
replicateM : Int -> State s a -> State s (List a)
replicateM total s =
    let
        go ( n, xs ) =
            if n < 1 then
                state (Done xs)

            else
                map (\x -> Loop ( n - 1, x :: xs )) s
    in
    tailRecM go ( total, [] )


zipWithM : (a -> b -> State s c) -> List a -> List b -> State s (List c)
zipWithM f ps qs =
    combine (List.map2 f ps qs)


mapAndUnzipM : (a -> State s ( b, c )) -> List a -> State s ( List b, List c )
mapAndUnzipM f xs =
    map List.unzip (traverse f xs)



-- Tail Recursion - based on purescript https://github.com/purescript/purescript-tailrec/blob/master/src/Control/Monad/Rec/Class.purs


{-| The result of a compuation: either `Loop` containing the updated accumulator,
or `Done` containing the final result of the computation.
-}
type Step a b
    = Loop a
    | Done b


{-| Create a pure tail-recursive function of one argument

    pow : number -> Int -> number
    pow n p =
        let
            go { accum, power } =
                if power == 0 then
                    Done accum

                else
                    Loop
                        { accum = accum * n
                        , power = power - 1
                        }
        in
        tailRec go { accum = 1, power = p }

-}
tailRec : (a -> Step a b) -> a -> b
tailRec f =
    let
        go step =
            case step of
                Loop a ->
                    go (f a)

                Done b ->
                    b
    in
    go << f


tailRecM2 : (a -> c -> State s (Step ( a, c ) b)) -> a -> c -> State s b
tailRecM2 f a b =
    tailRecM (\( x, y ) -> f x y) ( a, b )


{-| The `tailRecM` function takes a step function and applies it recursively until
a pure value of type `b` is found. Because of tail recursion, this function runs in constant stack-space.

    {-| Perform an action n times, gathering the results
    -}
    replicateM : Int -> State s a -> State s (List a)
    replicateM n s =
        let
            go ( n, xs ) =
                if n < 1 then
                    state (Done xs)

                else
                    map (\x -> Loop ( n - 1, x :: xs )) s
        in
        tailRecM go ( n, [] )

-}
tailRecM : (a -> State s (Step a b)) -> a -> State s b
tailRecM f a =
    let
        helper : ( Step a b, c ) -> Step ( a, c ) ( b, c )
        helper ( m, s1 ) =
            case m of
                Loop x ->
                    Loop ( x, s1 )

                Done y ->
                    Done ( y, s1 )

        step : ( a, s ) -> Step ( a, s ) ( b, s )
        step ( value, s ) =
            case f value of
                State st ->
                    helper (st s)
    in
    State <| \s -> tailRec step ( a, s )
