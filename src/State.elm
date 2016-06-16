module State exposing 
    ( State(..), state
    , map, map2, map3
    , andMap, (<*>)
    , andThen, join
    , get, put, modify
    , runState, execState, evalState
    )

{-| This library helps with threading state through a calculation 

The motivating example is something like this 

```
import Random exposing (Generator, Seed, int) 

threeRandomInts : Seed -> (Int, Int, Int)
threeRandomInts seed = 
    let generator = Generator (int 0 10)
        (value1, newSeed)   = Random.step generator seed
        (value2, newerSeed) = Random.step generator newSeed
        (value3, _)         = Random.step generator newerSeed
    in 
        (value1, value2, value3)
```

When you want to use an updated version of a value multiple times, this library comes in handy.
The above can be written as 

```
threeRandomInts : Seed -> (Int, Int, Int)
threeRandomInts seed = 
    let 
        generator = State (Random.step (Random.int 0 10))
    in 
        (state (,,)) `andMap` (generator) `andMap` (generator) `andMap` (generator) 
            |> (flip evalState) seed
```

This scales much nicer as you need to use the updated value more often.

#Type and Constructors
@docs State, state

#Mapping
@docs map, map2, map3

#Applying
@docs andMap, (<*>)

#Chaining 
@docs andThen, join

#Changing State
@docs get, put, modify

#Running State
@docs runState, evalState, execState
-} 

-- Type and Constructors

{-| Type that represents state. 
-}
type State state value = 
   State (state -> (value, state))
  
{-| Create a new State from a value of any type. 
-}
state : value -> State state value
state value = State (\s -> (value, s))


{-| Apply a function to the value that the state holds 
-} 
map : (a -> b) -> State s a -> State s b
map f (State step) = 
    State <| 
          (\ seed -> 
              let (value, newSeed) = step seed
              in 
                  (f value, newSeed) 
          )
  
{-| Apply a function to the value of two states. The newest state will be kept 
-}
map2 : (a -> b -> c) -> State s a -> State s b -> State s c
map2 f (State step1) (State step2) = 
    State <| 
  (\ seed -> 
      let (value1, newSeed)   = step1 seed
          (value2, newerSeed) = step2 newSeed
      in 
          (f value1 value2, newerSeed) 
  )

{-| Apply a function to the value of three states. The newest state will be kept

The definition of map3 is in terms of andMap, which can be used to create
map4, map5 ect.

```
map3 : (a -> b -> c -> d) -> State s a -> State s b -> State s c -> State s d
map3 f step1 step2 step3 =  
    (state f) `andMap` step1 `andMap` step2 `andMap` step3
```
-}
map3 : (a -> b -> c -> d) -> State s a -> State s b -> State s c -> State s d
map3 f step1 step2 step3 =  
    (state f) <*> step1 <*> step2 <*> step3

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
andMap = map2 (<|)

{-| Infix version of andMap, the operation is left-associative with precedence level 5.
-}
(<*>) : State s (a -> b) -> State s a -> State s b
(<*>) = andMap

infixl 5 <*>

-- Chaining

{-| Chain two operations with state. 
-}
andThen : State s a -> (a -> State s b) -> State s b
andThen (State h) f =  
    State <| \s -> 
        let (a, newState) = h s
            (State g) = f a 
        in 
            g newState

{-| Throws away a level of state 
-}
join : State s (State s a) -> State s a
join value = value `andThen` identity

{-| Get the current state. Typically the state is 
modified somehow and then put back with put.

An example with get and set

```
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
```
-}
get : State s s
get = State (\s -> (s, s))

{-| Replace the current state with a new one
-}
put : s -> State s ()
put x = State (\s -> ((), x))


{-| Modify the state. This is a combination of set and put
-}
modify : (s -> s) -> State s ()
modify f = 
   get `andThen` \x -> put (f x)


{-| Thread the state through a computation, 
and return both the final state and the computed value -}
runState : State s a -> s -> (a, s)
runState (State h) initialState = h initialState 

{-| Thread the state through a computation, 
and return only the computed value -}
evalState : State s a -> s -> a
evalState state = fst << runState state

{-| Thread the state through a computation, 
and return only the final state -}
execState : State s a -> s -> s
execState state = snd << runState state


