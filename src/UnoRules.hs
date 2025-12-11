module UnoRules where

import DSL
import Logic
import Types

stackPenaltyMove :: Transition
stackPenaltyMove =
  Transition
    { fromState = CheckPenalty,
      toState = ExecuteCard,
      description = "Stack Penalty (+2/+4)",
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
      description = "Confirm No Penalty",
      condition = \st _ -> pendingPenalty st == 0,
      effect = \_ -> return ()
    }

validateMove :: Transition
validateMove =
  Transition
    { fromState = WaitForInput,
      toState = ExecuteCard,
      description = "Play Valid Card",
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
      description = "Draw Card",
      condition = \_ act -> act == DrawCard,
      effect = drawCard
    }

checkUnoMove :: Transition
checkUnoMove =
  Transition
    { fromState = ExecuteCard,
      toState = CheckUnoShout,
      description = "Check UNO Shout",
      condition = \_ _ -> True,
      effect = checkUno
    }

victoryMove :: Transition
victoryMove =
  Transition
    { fromState = CheckUnoShout,
      toState = GameOver,
      description = "Declare Victory",
      condition = \st _ -> null (hand (players st !! currentPlayerIndex st)),
      effect = \_ -> return ()
    }

noVictoryMove :: Transition
noVictoryMove =
  Transition
    { fromState = CheckUnoShout,
      toState = ApplyEffect,
      description = "Apply Card Effects",
      condition = \st _ -> not (null (hand (players st !! currentPlayerIndex st))),
      effect = applySpecialCardEffect
    }

effectsDoneMove :: Transition
effectsDoneMove =
  Transition
    { fromState = ApplyEffect,
      toState = SwitchTurn,
      description = "Effects Applied",
      condition = \_ _ -> True,
      effect = \_ -> return ()
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
unoMachine =
  [ stackPenaltyMove,
    acceptPenaltyMove,
    noPenaltyMove,
    validateMove,
    drawMove,
    checkUnoMove,
    victoryMove,
    noVictoryMove,
    effectsDoneMove,
    switchTurnMove
  ]