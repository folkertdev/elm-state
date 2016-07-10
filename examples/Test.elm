module Main exposing (..)

import State exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Random exposing (Seed, initialSeed)
import Debug
import String
import Char


{- This is a testing/playground file. Functions found in this this file
   are experimental and therefore not necessarily a good idea.
-}


flipCoin : State Seed Bool
flipCoin =
    advance (Random.step Random.bool)


threeRandomInts : Seed -> ( Int, Int, Int )
threeRandomInts seed =
    let
        generator =
            State (Random.step (Random.int 0 10))
    in
        (,,)
            `map` generator
            `andMap` generator
            `andMap` generator
            |> State.finalValue seed


randint : State Seed Int
randint =
    Random.int 0 100
        |> Random.step
        |> advance


lowercase : State Seed Char
lowercase =
    Random.int 97 122
        |> Random.map Char.fromCode
        |> Random.step
        |> advance


( value, finalSeed ) =
    State.run (Random.initialSeed 5) randint
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { dieFace : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model 1, Cmd.none )



-- UPDATE


type Msg
    = Roll
    | NewFace Int
    | Message String


replace : State s a -> State s b -> State s b
replace s new =
    s `andThen` \_ -> new


multiple : State Model (Cmd Msg)
multiple =
    let
        swap ( a, b ) =
            ( b, a )

        messages : List Msg
        messages =
            [ NewFace 4, Message "I'm from the messages list", NewFace 6 ]

        step =
            advance << (\f arg -> swap (f arg)) << update

        advances =
            List.map step messages
    in
        (state Cmd.none)
            |> (flip <| List.foldr (map2 mappend)) advances


mappend : Cmd msg -> Cmd msg -> Cmd msg
mappend cmd1 cmd2 =
    Cmd.batch [ cmd1, cmd2 ]



--    |> (flip State.run model)
--   |> swap


swap ( a, b ) =
    ( b, a )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            let
                ( newModel, commands ) =
                    swap <| State.run model multiple
            in
                ( newModel, mappend commands <| Random.generate NewFace (Random.int 0 10) )

        NewFace newFace ->
            let
                randval =
                    List.foldl (map2 (String.cons)) (state "") (List.repeat 10 lowercase)
                        |> State.finalValue (initialSeed newFace)

                _ =
                    Debug.log "randval" randval
            in
                ( Debug.log "newFace" model
                , Random.generate (Message << toString) (Random.int 0 10)
                )

        Message v ->
            let
                _ =
                    Debug.log ("got " ++ toString v ++ " from NewFace") ()
            in
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (toString model.dieFace) ]
        , button [ onClick Roll ] [ text "Roll" ]
        ]
