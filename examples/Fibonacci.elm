module Fibonacci exposing (fib, fibs)

import State exposing (State, modify, andThen)
import Cache
import Dict exposing (Dict)
import Html exposing (Html, text)


initialState : Dict Int Int
initialState =
    Dict.fromList [ ( 0, 1 ), ( 1, 1 ) ]


fib : Int -> Int
fib =
    State.finalValue initialState << fibHelper


{-| Generates the nth fibonacci number recursively
-}
nextFib : Int -> State (Dict Int Int) Int
nextFib n =
    State.map2 (+) (fibHelper (n - 1)) (fibHelper (n - 2))


{-| Here is where State's magic comes in:

When the nth fibonacci number is already in the Dict that the State contains,
it will not be generated. Rather, the cached value is used.

If the number is not in the Dict, it is calculated with nextFib, put in the Dict
and then inserted as the value.

The heavy lifting of keeping track of the cache is completely hidden
from the programmer
-}
fibHelper : Int -> State (Dict Int Int) Int
fibHelper n =
    Cache.generateIfMissing nextFib n


fibs : List Int -> List Int
fibs =
    State.finalValue initialState << fibsHelper


fibsHelper : List Int -> State (Dict Int Int) (List Int)
fibsHelper =
    -- State.traverse makes sure to reuse the cache between calls to fibHelper.
    State.traverse fibHelper


main =
    fibs [0..9]
        |> toString
        |> text
