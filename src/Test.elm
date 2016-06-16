import Html exposing (..)
import Random exposing (Seed, Generator, bool, initialSeed)
import String

import State exposing (..)

type alias ID = Seed

type alias Record = 
  { a : Int
  , b : String
  , c : Int
  , d : Maybe String
  } 

-- step : gen -> (id, gen)

monadic : Seed -> Record 
monadic = 
    evalState <| (State step) 
        `andThen` \a -> 
            state 3 
        `andThen` \b -> 
            state (Record a "" b Nothing) 


threeRandomInts : Seed -> (Int, Int, Int)
threeRandomInts seed = 
    let 
        generator = State (Random.step (Random.int 0 10))
    in 
        (state (,,)) `andMap` (generator) `andMap` (generator) `andMap` (generator) 
            |> (flip evalState) seed
            
  
myValue : Seed -> Record
myValue initialGenerator = 
  let constructor = 
    state (Record 1) 
      <*> State.map toString (State step)
      <*> state 3
      <*> State.map (Just << toString) (State step)
   in 
     evalState constructor initialGenerator


generator = Random.int 0 10 


step : Seed -> (Int, Seed)
step = Random.step generator

pure v = (\seed -> (v, seed))





mapLeft f (x, y) = (f x, y)


  
main = text <| toString <| myValue (initialSeed 5)
