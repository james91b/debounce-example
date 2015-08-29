module Main where

import Signal
import Time exposing (Time, millisecond)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Never, Effects)
import Task
import StartApp

-- MODEL
type alias Model = { query : String, results : Maybe String,  debounceState : DebounceState }
type alias DebounceState = Maybe { prevClockTime : Time,  elapsedTime: Time }

init : (Model, Effects Action)
init = ( { query = "", results = Nothing, debounceState = Nothing }, Effects.none )

-- UPDATE
type Action = Input String 
            | UpdateResults (Maybe String)
            | Tick Time

elapsedTimeOf : DebounceState -> Time -> Time
elapsedTimeOf debounceState clockTime = 
  case debounceState of
    Nothing -> 0
    Just {elapsedTime, prevClockTime} -> elapsedTime + (clockTime - prevClockTime)

tickDebounceState : Model -> Time -> Time -> Model
tickDebounceState model newElapsedTime clockTime = 
  { model | debounceState <- Just { elapsedTime = newElapsedTime, prevClockTime = clockTime} }

debounceTime : Time
debounceTime = 500*millisecond

update : Action -> Model -> (Model, Effects Action)
update msg model =
  case msg of
    Input query -> 
      case model.debounceState of
        Nothing -> ({model | query <- query}, Effects.tick Tick)
        Just oldDebounceState -> ({model | query <- query, debounceState <- Nothing}, Effects.none)
    UpdateResults results -> 
      ({model | results <- results}, Effects.none)
    Tick clockTime -> 
      let
        newElapsedTime = elapsedTimeOf model.debounceState clockTime
      in
        if newElapsedTime > debounceTime then
          ({model | debounceState <- Nothing}, search model.query)
        else
          (tickDebounceState model newElapsedTime clockTime, Effects.tick Tick)

search : String -> Effects Action
search query = Task.succeed ("Results of \"" ++ query ++ "\"") 
             |> Task.toMaybe 
             |> Task.map UpdateResults 
             |> Effects.task

-- VIEW
toResult : Maybe String -> List Html
toResult results = case results of 
                     Just result -> [text result]
                     Nothing -> []
                                
view : Signal.Address Action -> Model -> Html
view address model = div 
                     [] 
                     [ 
                      input [ type' "text"
                            , on "input" targetValue (Signal.message (Signal.forwardTo address Input))
                            ] []
                     , p [] (toResult model.results)
                     ]
                     


--WIRING
app = StartApp.start
      { init = init
      , update = update
      , view = view
      , inputs = []
      }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

