module DSL where

import Data.List (find)
import Types

type Condition = GameState -> PlayerAction -> Bool

type Effect = GameState -> PlayerAction -> GameState

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
      let stateAfterEffect = effect rule state action
          finalState = stateAfterEffect {currentPhase = toState rule}
       in Right finalState
    Nothing ->
      Left "Impossible move: there is no suitable transition"
  where
    isMatchingRule rule =
      fromState rule == currentPhase state && condition rule state action
