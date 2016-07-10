module State
    exposing
        ( State(..)
        , state
        , map
        , map2
        , map3
        , mapState
        , filterState
        , foldState
        , andMap
        , (<*>)
        , andThen
        , join
        , get
        , put
        , modify
        , advance
        , run
        , finalState
        , finalValue
        )

{-| This library helps with threading state through a calculation

This is extremely convenient when composing with functions of the type `a -> (b, a)`, like
Random.step or (with a bit of squinting) the TEA update function.

The motivating example is something like this
```
import Random exposing (Generator, Seed, int)

threeRandomInts : Seed -> (Int, Int, Int)
threeRandomInts seed =
    let
        generator = Generator (int 0 10)
        (value1, newSeed)   = Random.step generator seed
        (value2, newerSeed) = Random.step generator newSeed
        (value3, _)         = Random.step generator newerSeed
    in
        (value1, value2, value3)
```

The threading of state is done with tuple unpacking. This requires an extra variable name for
every step you make. It is quite likely that you make typos in this repetitive code (for instance reusing newSeed twice)
resulting in nasty bugs.

Using State, the above can be written as

```
threeRandomInts : Seed -> (Int, Int, Int)
threeRandomInts seed =
    let
        generator = State (Random.step (Random.int 0 10))
    in
        (,,) `map` generator `andMap` generator `andMap` generator
            |> evalState seed
```

Using state, there is less code and less space for typos.

#Type and Constructors
@docs State, state

#Mapping
@docs map, map2, map3

#Generalized Mapping
@docs mapState, filterState, foldState

#Applying
@docs andMap, (<*>)

#Chaining
@docs andThen, join

#Changing State
@docs get, put, modify, advance

#Running State
@docs run, finalValue, finalState

#Notes for the haskellers/curious

Yes, it is the State monad (a type with andThen defined on it). This fact is not terribly important
for elm users. Monads (and other concepts from category theory) are a big part of what makes
functional programming great, but the magical aura that hangs around these terms is not a part of "the best of
functional programming", elm's mission statement. I've therefore chosen to leave "the M-word" out of the rest of
the documentation
-}

-- Type and Constructors


{-| Type that represents state.

Note that `State` describes a function, not a concrete value.
-}
type State state value
    = State (state -> ( value, state ))


{-| Create a new State from a value of any type.
-}
state : value -> State state value
state value =
    State (\s -> ( value, s ))


{-| Apply a function to the value that the state holds
-}
map : (a -> b) -> State s a -> State s b
map f (State step) =
    State
        <| (\seed ->
                let
                    ( value, newSeed ) =
                        step seed
                in
                    ( f value, newSeed )
           )


{-| Apply a function to the value of two states. The newest state will be kept
-}
map2 : (a -> b -> c) -> State s a -> State s b -> State s c
map2 f (State step1) (State step2) =
    State
        <| (\seed ->
                let
                    ( value1, newSeed ) =
                        step1 seed

                    ( value2, newerSeed ) =
                        step2 newSeed
                in
                    ( f value1 value2, newerSeed )
           )


{-| Apply a function to the value of three states. The newest state will be kept

The definition of map3 is in terms of andMap, which can be used to create
map4, map5 ect.

```
map3
    : (a -> b -> c -> d)
    -> State s a
    -> State s b
    -> State s c
    -> State s d
map3 f step1 step2 step3 =
    f `andMap` step1 `andMap` step2 `andMap` step3
```
-}
map3 : (a -> b -> c -> d) -> State s a -> State s b -> State s c -> State s d
map3 f step1 step2 step3 =
    f `map` step1 <*> step2 <*> step3


{-| Generalize `List.map` to work with `State`.

When you have a function the works on a single element,

    mark : Int -> State (Array Bool) ()
    mark index =
        State.modify (Array.set index False)

mapState can be used to let it work on a list of elements,
taking care of threading the state through.

    markMany : List Int -> State (Array Bool) (List ())
    markMany = State.mapState mark

This function is also called `mapM`.
-}
mapState : (a -> State s b) -> List a -> State s (List b)
mapState f list =
    let
        folder elem accum =
            map2 (::) (f elem) accum
    in
        List.foldr folder (state []) list


{-| Generalize `List.filter` to work on `State`. Also called `filterM`.
-}
filterState : (a -> State s Bool) -> List a -> State s (List a)
filterState p =
    let
        keepWhen current keep =
            if keep then
                (\e -> current :: e)
            else
                identity

        folder current =
            map2 (keepWhen current) (p current)
    in
        List.foldr folder (state [])


{-| Compose a list of updated states into one. Also called `foldM`.
-}
foldState : (b -> a -> State s b) -> b -> List a -> State s b
foldState f initialValue list =
    let
        -- f' : c -> (c -> State s d) -> d -> State s d
        f' x k z =
            (f z x) `andThen` k
    in
        List.foldr f' state list <| initialValue



{-
   case elements of
       [] ->
           state initialValue

       x :: xs ->
           (f initialValue x) `andThen` \newInitial -> foldState f newInitial xs
-}
-- Applying


{-| Apply a function wrapped in a state to a value wrapped in a state.
This is very useful for applying stateful arguments one by one.

```
twoRandomInts : Seed -> (Int, Int)
twoRandomInts seed =
    let generator = Generator (int 0 10)
    in
        evalState (state (,) `andMap` (step generator) `andMap` (step generator)) seed
```

andMap is defined in terms of map2
```
andMap = map2 (<|)
```
-}
andMap : State s (a -> b) -> State s a -> State s b
andMap =
    map2 (<|)


{-| Infix version of andMap, the operation is left-associative with precedence level 4.
-}
(<*>) : State s (a -> b) -> State s a -> State s b
(<*>) =
    andMap



-- 4 is also haskell's precedent level for <*>


infixl 4 <*>



-- Chaining


{-| Chain two operations with state. See <a href="#get">get</a> for an example.
-}
andThen : State s a -> (a -> State s b) -> State s b
andThen (State h) f =
    let
        operation s =
            let
                ( a, newState ) =
                    h s

                (State g) =
                    f a
            in
                g newState
    in
        State operation


{-| Throws away a level of state
-}
join : State s (State s a) -> State s a
join value =
    value `andThen` identity



-- changing the state


{-| Get the current state. Typically the state is
modified somehow and then put back with put.

An example with get and set

    flipCoin : State Seed Bool
    filpCoin =
        get
            `andThen` \seed ->
                let (value, newSeed) = Random.step Random.bool seed
                in put newSeed
            `andthen` \_ ->
                state value

    twoFlips : State Seed (Bool, Bool)
    twoFilps = map2 (,) flipCoin flipCoin
-}
get : State s s
get =
    State (\s -> ( s, s ))


{-| Replace the current state with a new one
-}
put : s -> State s ()
put x =
    State (\s -> ( (), x ))


{-| Modify the state. This is a combination of set and put
-}
modify : (s -> s) -> State s ()
modify f =
    get `andThen` \x -> put (f x)


{-| Advance the current state into a value and a new state.

The function signature looks a little funny: it seems to create a value and a state
out of thin air. Remember that the state and the value are not actually within a
`State` object, as `State s a` is just a wrapper around a function of type `s -> (a, s)`

    flipCoin : State Seed Bool
    filpCoin =
        advance (Random.step Random.bool)
-}
advance : (s -> ( a, s )) -> State s a
advance f =
    get
        `andThen` \x ->
                    let
                        ( value, newState ) =
                            f x
                    in
                        put newState
                            `andThen` \_ ->
                                        state value


{-| Thread the state through a computation,
and return both the final state and the computed value

Note for Haskellers: the argument order is swapped. This is more
natural in elm because code is often structured left to right using `(|>)`.

    randint : State Seed Int
    randint =
        Random.int 0 100
            |> Random.step
            |> advance

    (value, finalSeed) =
        run (Random.initialSeed 42) randint
-}
run : s -> State s a -> ( a, s )
run initialState (State s) =
    s initialState


{-| Thread the state through a computation,
and return only the computed value

Generating a string of 10 random lowercase letters can be done as follows:

    lowercase : State Seed Char
    lowercase =
        Random.int 97 122
            |> Random.map Char.fromCode
            |> Random.step
            |> advance

    randomString =
        let letters = List.repeat 10 lowercase
        in
            List.foldl (map2 String.cons) (state "") letters
                |> finalValue (initialSeed 42)
-}
finalValue : s -> State s a -> a
finalValue initialState =
    fst << run initialState


{-| Thread the state through a computation,
and return only the final state
-}
finalState : s -> State s a -> s
finalState initialState =
    snd << run initialState
