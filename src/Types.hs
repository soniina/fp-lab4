module Types where

data Color = Red | Yellow | Green | Blue
  deriving (Show, Eq)

data Value
  = Number Int -- 0..9
  | Skip
  | Reverse
  | DrawTwo
  | Wild
  | WildDrawFour
  deriving (Show, Eq)

data Card = Card
  { color :: Maybe Color,
    value :: Value
  }
  deriving (Show, Eq)

type Deck = [Card]

data Player = Player
  { name :: String,
    hand :: [Card]
  }
  deriving (Show)

data GamePhase
  = WaitForInput
  | Validation
  | ExecuteCard
  | ApplyEffect
  | CheckUnoShout
  | CheckVictory
  | SwitchTurn
  | GameOver
  deriving (Show, Eq, Ord)

data PlayerAction
  = PlayCard Int Bool
  | PlayWildCard Int Color Bool
  | DrawCard
  deriving (Show, Eq)

data GameState
  = GameState
  { players :: [Player],
    deck :: Deck,
    discardPile :: [Card],
    activeColor :: Maybe Color,
    currentPlayerIndex :: Int,
    direction :: Int, -- 1 (по часовой) или -1 (против)
    currentPhase :: GamePhase,
    pendingPenalty :: Int,
    gameLog :: [String]
  }
  deriving (Show)
