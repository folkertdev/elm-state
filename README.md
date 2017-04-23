Threading state through computation
===================================

This library provides ways to compose functions of the type 
`s -> (a, s)`. 

## <a name="motivation">Motivation</a>

From time to time, you'll see a pattern like this in your code 

```elm
(newValue,     newState)     = f state
(newerValue,   newerState)   = g newValue   newState
(newererValue, newererState) = h newerValue newerState
``` 

The above pattern is ugly and error-prone (because of typo's, for instance). 
It can be abstracted by creating a function that composes `f` and `g`, that is 
the output of `f` is the input to `g`.

```elm
    f :      s -> (a, s) 
    g : a -> s -> (a, s) 
```

This library implements the described composition and provides a bunch of helper functions for 
working with State. For a more in-depth explanation of how the implementation works, see the [derivation](#derivation).

*This library is advanced and relatively abstract. 
If anything is unclear, please [open an issue](https://github.com/folkertdev/elm-state/issues)*.

## Working with values within State 

There are three main functions for working with the wrapped state value: 

* **State.get**: aquire the state value.
    ```elm
    State.map2 (+) State.get State.get
        |> State.run 42
        -- == (84, 42)
    ```

* **State.put**: set the state to a certain value.
    ```elm
    State.put 5
        |> State.run 3
        -- == ((), 5)
    ```
    Note that `State.put` discards the value currently stored in the state. 

* **State.modify**: apply a function to the state; a combination of get and set
    ```elm
    State.modify (\v -> v + 1)
        |> State.map (\_ -> "finished")
        |> State.run 42
        -- == ("finished", 43)
    ```
    Note that `State.modify` (because it combines get and put) discards
    the value currently stored in the state.
    

## Extracting results from State

Remember, we really build a large `s -> (a, s)` function out of smaller components. To run this function, 
we need an initial state, and get a final state and a final value. The function `State.run` does just that.

```elm
run : s -> State s a -> (a, s)
run initialState (State f) = 
    f initialState
```

## Structuring computation with andThen

The composition operator for functions wrapped in `State` is called `andThen`. It is the primary way 
to structure computations that involve `State`. When not used with care, this can lead to truly awful code.
This is what the Haskellers at Facebook call [a code tornado](https://youtu.be/mlTO510zO78?t=34m7s):

```elm
cycle : Int -> State (Array Bool) (Maybe Int)
cycle n =
    let
        mark : Int -> State (Array Bool) ()
        mark n =
            State.modify (Array.set n False)

        multiplesToMark length n =
            range (n * n) length n

    in
        State.map Array.length State.get
            |> andThen (\length ->  
                State.mapState mark (multiplesToMark length n)
                    |> andThen  (\_ -> 
                        State.map (toNextIndex n) State.get
                    )
            )
```

This problem can be solved by extracting subcomputations and giving them a descriptive name. Not only is the final composition
(in the 'in' clause) very readable and understandable, the individual components are also nicely reusable.

```elm
cycle : Int -> State (Array Bool) (Maybe Int)
cycle n =
    let
        mark : Int -> State (Array Bool) ()
        mark n =
            State.modify (Array.set n False)

        multiplesToMark length n =
            range (n * n) length n

        getArrayLength =
            State.map Array.length State.get

        markMultiples length =
            -- mapState shares the Array Int between invocations of mark.
            State.mapState mark (multiplesToMark length n)

        setNextIndex _ =
            State.map (toNextIndex n) State.get
    in
        getArrayLength 
            |> andThen markMultiples 
            |> andThen setNextIndex
```

When using andThen, try to break up your computation into small, reusable bits and give them a descriptive name. 
A general Elm principle is that the shortest code is often not the best code. Don't take shortcuts with
andThen in production code.



### Tips 

* **Prevent code tornadoes**
* **Name subcomputations/functions appropriately**
* **A bit more verbose is better than a bit shorter**
* **Limit the amount of code that is "in State" to a minimum**
    Try to keep functions pure and use the helper functions in this package
    to let the work on values "in State". 
* **Limit the use of andThen in combination with State.state** 
    Instead of this 
    ```elm
    State.get |> andThen (\value -> state (f value))
    ```

    write 
    ```elm
    State.map f State.get
    ```

# Use cases  

By design, most functional programs don't use state, because it's quite cumbersome to work with. 
It's a good idea to see whether you can do without this library.

Sometimes though, there are very good reasons. This is mostly the case in traditionally imperative algorithms that use
nonlocal mutable variables. There are a few other cases in standard Elm where the pattern in the (motivation)[#movation]
pops up, the primary ones being working with random values and updating (child) components. This library is not made 
for the latter purposes, but its concepts transfer over. 

Finally, there is a pattern from Haskell that uses State to hold configuration information (on its own called Read) and 
to store logging information (on its own called Write). This pattern hasn't really found its way to Elm, 
and it may not need to, because Elm solves its problems differently. In any case, experiment and see what works for you. 

## Caching function results 

Some computations are resource-intensive and should preferably only be performed once. The classical example 
of this is the fibonacci sequence. 

```elm
fib : Int -> Int 
fib n = 
    case n of 
        0 -> 1
        1 -> 1
        _ -> fib (n - 1) + fib (n - 2)
```

Every evaluation of `fib` (except the base cases 0 and 1) requires two more evaluations. 
Those evaluations each too require two more evaluations, making the time complexity of this function exponential. 
Furthermore, the two trees (`fib (n - 1)` and `fib (n - 2)`) overlap, doing the same calculations twice. 
These problems can we solved by caching already calculated values. This is where `State` comes in:

```elm
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
        -- takes n as an argument to preserve laziness
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
```

Notice how, with the help of choosing descriptive names, this function reads like a 
step-by-step description of what happens. This is what makes using `andThen` so nice. 

It is also possible to reuse the cache for multiple invocations of fibHelper, for example using `State.traverse`.

```elm 
fibs : List Int -> List Int
fibs =
    let
        initialState =
            Dict.fromList [ ( 0, 1 ), ( 1, 1 ) ]
    in
        State.finalValue initialState << fibsHelper


fibsHelper : List Int -> State (Dict Int Int) (List Int)
fibsHelper =
    State.traverse fibHelper
```


## Threading a Random seed

When working with random values, you have to update the seed after every computation.
Note how this is very similar to the general pattern in the [motivation](#motivation) section.

```elm
myRandomValues =
    let
        myGenerator = Random.int 0 10

        (a, newSeed1) = Random.step myGenerator seed
        (b, newSeed2) = Random.step myGenerator newSeed1
        (c, newSeed3) = Random.step myGenerator newSeed2
    in
        (a, b, c)
```

The `Random.mapN` functions do exactly what `compose` described above does. I advice to 
use the random-specific functions when working with random values. 

```elm
myRandomValues =
    let
        myGenerator = Random.int 0 10
    in
        Random.map3 (,,) myGenerator myGenerator myGenerator
            |> (flip Random.step) seed
            |> Tuple.first
```

## Recursively applying update

**Don't use this library for this purpose, prefer [Fresheyeball/elm-return](http://package.elm-lang.org/packages/Fresheyeball/elm-return/latest)**.

The typical signature for update with TEA is 

    update : Msg -> Model -> (Model, Cmd Msg)

When we drop the first argument (partial function application) and swap the order 
of the types in the tuple, we get

    update' : Model -> (Cmd Msg, Model) 

This is a function with the same type as the ones wrapped in State. 
The [elm-effects](http://package.elm-lang.org/packages/akbiggs/elm-effects/latest/Effects) library uses 
a pattern similar to this library to consecutively apply operations that generate side-effects to a value. 

Elm-effects is a bit less powerful than this package (technically, it's just a Writer monad, not a State monad). 
If you need the more powerful functions then reevaluate, use this package and maybe contribute to elm-effects.


# <a name="derivation">Derivation</a>

A derivation of how the composition of functions of the form `s -> (a, s)` works.

In the example from the beginning,

```elm
(newValue,     newState)     = f state
(newerValue,   newerState)   = g newValue   newState
(newererValue, newererState) = h newerValue newerState
``` 

`f` and `g` have the following types.

```elm
    f :      s -> (a, s) 
    g : a -> s -> (a, s) 
```

We want to compose these two functions, which means that the `a` and `s` returned by `f` serve as 
an input to `g`. This can be done as follows:

```elm
    composed : a -> s -> (s, a) 
    composed value state = 
        let 
            (newValue, newState) = f value state
        in 
            g newValue newState
```

Let's generalize this a little further

```elm
    compose :       (s -> (a, s))
            -> (a -> s -> (b, s))
            ->      (s -> (b, s))
    compose f g state = 
        let 
            (newValue, newState) = f state
        in 
            g newValue newState
```

The essence of this library is composing multiple functions of the type `s -> (a, s)` into one giant function of the same type.
All the intermediate threading (result of `f` is input to `g`) is automatically taken care of. For better error messages, 
the `s -> (a, s)` function is wrapped in a type.

```elm
type State s a = 
    State (s -> (a, s))
```

Compose can then be written as 
```elm
compose : State s a -> (a -> State s b) -> State s b
compose (State f) g = 
    let 
        helper : s -> (s, b)
        helper initialState = 
            let (newValue, newState) = f initialState

                -- unwrap the newly created state
                (State h) = g newValue
            in 
                h newState
    in 
        State helper 
```

Functions of the type `(a -> Wrapper b) -> Wrapper a -> Wrapper b` are called `andThen` in Elm (see
[Maybe.andThen](http://package.elm-lang.org/packages/elm-lang/core/latest/Maybe#andThen) and
[Random.andThen](http://package.elm-lang.org/packages/elm-lang/core/latest/Random#andThen)), but sometimes
also referred to as bind or `(>>=)`.



# Problems 

* **RangeError: Recursion limit exceeded**: Older versions of this package would often throw this error at 
runtime. Most of these cases are now fixed, but if you run into this error, please report it.  





