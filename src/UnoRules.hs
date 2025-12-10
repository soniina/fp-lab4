module UnoRules where

import DSL
import Logic
import Types

stackPenaltyMove :: Transition
stackPenaltyMove =
  Transition
    { fromState = CheckPenalty,
      toState = ExecuteCard,
      description = "Counter-Attack! Stack +2/+4",
      condition = \st act ->
        case act of
          PlayCard cardId _ ->
            let card = getCardFromHand st cardId
                top = getTopCard st
             in pendingPenalty st > 0
                  && value card == value top
                  && value top == DrawTwo
          PlayWildCard cardId _ _ ->
            let card = getCardFromHand st cardId
                top = getTopCard st
             in pendingPenalty st > 0
                  && value card == value top
                  && value top == WildDrawFour
          _ -> False,
      effect = playCard
    }

acceptPenaltyMove :: Transition
acceptPenaltyMove =
  Transition
    { fromState = CheckPenalty,
      toState = SwitchTurn,
      description = "Accept Penalty (Draw cards)",
      condition = \st act ->
        pendingPenalty st > 0 && act == DrawCard,
      effect = acceptPenalty
    }

noPenaltyMove :: Transition
noPenaltyMove =
  Transition
    { fromState = CheckPenalty,
      toState = WaitForInput,
      description = "No penalty, play normal",
      condition = \st _ -> pendingPenalty st == 0,
      effect = \_ -> return ()
    }

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

applyEffectMove :: Transition
applyEffectMove =
  Transition
    { fromState = ExecuteCard,
      toState = ApplyEffect,
      description = "Apply Card Effect",
      condition = \_ _ -> True,
      effect = applySpecialCardEffect
    }

switchTurnMove :: Transition
switchTurnMove =
  Transition
    { fromState = SwitchTurn,
      toState = CheckPenalty,
      description = "Switch Player",
      condition = \_ _ -> True,
      effect = switchTurn
    }

unoMachine :: GameMachine
unoMachine = [validateMove, drawMove]