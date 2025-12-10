module UnoRules where

import DSL
import Logic
import Types

validateMove :: Transition
validateMove =
  Transition
    { fromState = WaitForInput,
      toState = ExecuteCard,
      description = "User plays a card",
      effect = playCard,
      condition = \st act ->
        case act of
          PlayCard idx _ ->
            isValidMove (getCardFromHand st idx) (getTopCard st) (activeColor st)
          PlayWildCard idx _ _ ->
            isValidMove (getCardFromHand st idx) (getTopCard st) (activeColor st)
          _ -> False
    }

drawMove :: Transition
drawMove =
  Transition
    { fromState = WaitForInput,
      toState = SwitchTurn,
      description = "User draws a card",
      condition = \_ act -> act == DrawCard,
      effect = drawCard
    }

unoMachine :: GameMachine
unoMachine = [validateMove, drawMove]