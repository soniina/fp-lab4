module DSL (Transition (..), GameMachine, step) where

import Control.Monad.State (execState)
import Data.List (find)
import Types

type Condition = GameState -> PlayerAction -> Bool

type Effect = PlayerAction -> Game ()

data Transition = Transition
  { fromState :: GamePhase,
    toState :: GamePhase,
    condition :: Condition,
    effect :: Effect,
    description :: String
  }

type GameMachine = [Transition]

step :: GameMachine -> GameState -> PlayerAction -> Either String GameState
step machine state action =
  case find isMatchingRule machine of
    Just rule ->
      let stateAfterEffect = execState (effect rule action) state
          finalState = stateAfterEffect {currentPhase = toState rule}
       in Right finalState
    Nothing ->
      Left "Impossible move: there is no suitable transition"
  where
    isMatchingRule rule =
      fromState rule == currentPhase state && condition rule state action
