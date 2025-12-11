module Main where

import Control.Applicative ((<|>))
import Control.Monad.State
import DSL
import Data.Char (toLower)
import System.IO
import System.Random (newStdGen)
import Text.Read (readMaybe)
import Types
import UnoRules
import Utils (shuffle)

generateDeck :: [Card]
generateDeck = concat [numberCards, actionCards, wildCards]
  where
    colors = [Red, Yellow, Green, Blue]
    numberCards =
      concatMap
        ( \col ->
            Card (Just col) (Number 0)
              : concat [[Card (Just col) (Number n), Card (Just col) (Number n)] | n <- [1 .. 9]]
        )
        colors
    actionCards =
      concatMap
        ( \col ->
            concat [[Card (Just col) act, Card (Just col) act] | act <- [Skip, Reverse, DrawTwo]]
        )
        colors
    wildCards =
      replicate 4 (Card Nothing Wild)
        ++ replicate 4 (Card Nothing WildDrawFour)

setupGame :: IO GameState
setupGame = do
  gen <- newStdGen

  let (shuffledDeck, newGen) = shuffle generateDeck gen

  let (hand1, rest1) = splitAt 7 shuffledDeck
  let (hand2, rest2) = splitAt 7 rest1

  let playersWithCards =
        [ Player {name = "Player 1", hand = hand1},
          Player {name = "Player 2", hand = hand2}
        ]

  let (topCard : deckRest) = rest2

  return $
    GameState
      { players = playersWithCards,
        deck = deckRest,
        discardPile = [topCard],
        activeColor = color topCard <|> Just Red,
        currentPlayerIndex = 0,
        direction = 1,
        currentPhase = CheckPenalty,
        pendingPenalty = 0,
        gameLog = ["Game Started!"],
        rndGen = newGen
      }

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

showCard :: Card -> String
showCard (Card c v) =
  let colorStr = case c of
        Just Red -> "🔴 (Red)"
        Just Yellow -> "🟡 (Yellow)"
        Just Green -> "🟢 (Green)"
        Just Blue -> "🔵 (Blue)"
        Nothing -> "🌈 (Wild)"
      valStr = show v
   in valStr ++ " " ++ colorStr

parseCommand :: String -> GameState -> Either String PlayerAction
parseCommand input st =
  case words (map toLower input) of
    ["draw"] -> Right DrawCard
    ("play" : idStr : rest) -> do
      case readMaybe idStr of
        Nothing -> Left "Invalid card index. Use a number (e.g., 'play 0')."
        Just cardId -> do
          let pId = currentPlayerIndex st
          let pHand = hand (players st !! pId)
          if cardId < 0 || cardId >= length pHand
            then Left "Card index out of bounds!"
            else do
              let (chosenColor, saidUno) = parseRest rest (Nothing, False)
              let card = pHand !! cardId
              case value card of
                Wild -> case chosenColor of
                  Just col -> Right $ PlayWildCard cardId col saidUno
                  Nothing -> Left "⚠️ You must specify a color for Wild cards! (e.g., 'play 0 red')"
                WildDrawFour -> case chosenColor of
                  Just col -> Right $ PlayWildCard cardId col saidUno
                  Nothing -> Left "⚠️ You must specify a color for Wild cards! (e.g., 'play 0 blue')"
                _ -> Right $ PlayCard cardId saidUno
    _ -> Left "Unknown command format. Use 'play <id>' or 'draw'."
  where
    parseRest :: [String] -> (Maybe Color, Bool) -> (Maybe Color, Bool)
    parseRest [] acc = acc
    parseRest (w : ws) (mColor, mUno)
      | w == "uno" = parseRest ws (mColor, True)
      | w == "red" = parseRest ws (Just Red, mUno)
      | w == "yellow" = parseRest ws (Just Yellow, mUno)
      | w == "green" = parseRest ws (Just Green, mUno)
      | w == "blue" = parseRest ws (Just Blue, mUno)
      | otherwise = parseRest ws (mColor, mUno)

runGame :: GameState -> IO ()
runGame st = do
  if currentPhase st == GameOver
    then do
      clearScreen
      putStrLn "=========================================="
      putStrLn $ "🏆 WINNER: " ++ name (players st !! currentPlayerIndex st) ++ " 🏆"
      putStrLn "=========================================="
      return ()
    else do
      let isInteractive = case currentPhase st of
            WaitForInput -> True
            CheckPenalty -> pendingPenalty st > 0
            _ -> False

      if isInteractive
        then do
          interactionLoop st
        else do
          let result = step unoMachine st DrawCard
          case result of
            Right nextSt -> runGame nextSt
            Left err -> do
              putStrLn $ "CRITICAL ERROR in Auto-Phase (" ++ show (currentPhase st) ++ "): " ++ err
              interactionLoop st

interactionLoop :: GameState -> IO ()
interactionLoop st = do
  clearScreen
  let currentPlayer = players st !! currentPlayerIndex st

  putStrLn $ "=== TURN: " ++ name currentPlayer ++ " ==="
  when (pendingPenalty st > 0) $ putStrLn $ "⚠️ PENALTY ACTIVE: +" ++ show (pendingPenalty st)

  putStrLn "\n--- TABLE ---"
  let top = head (discardPile st)
  putStrLn $ "Top Card: " ++ showCard top
  case activeColor st of
    Just c -> putStrLn $ "Active Color: " ++ show c
    Nothing -> return ()

  putStrLn "\n--- YOUR HAND ---"
  let cards = zip [0 ..] (hand currentPlayer)
  mapM_ (\(i, c) -> putStrLn $ show i ++ ": " ++ showCard c) cards

  putStrLn "\n--- COMMANDS ---"
  if pendingPenalty st > 0
    then do
      putStrLn "draw                        -> Accept penalty cards"
      putStrLn "play <id> [color]           -> Stack penalty (Counter-attack!)"
    else do
      putStrLn "play <id>                   -> Play card (e.g., 'play 0')"
      putStrLn "play <id> <color>           -> Play Wild card (e.g., 'play 1 red')"
      putStrLn "play <id> [color] uno       -> Play and shout UNO!"
      putStrLn "draw                        -> Take a card from deck"

  putStr "\n> "
  hFlush stdout

  line <- getLine
  case parseCommand line st of
    Left parseError -> do
      putStrLn $ "\n" ++ parseError
      putStrLn "Press Enter..."
      _ <- getLine
      interactionLoop st
    Right action -> do
      case step unoMachine st action of
        Left _ -> do
          putStr "\n❌ MOVE REJECTED! "

          if pendingPenalty st > 0
            then putStrLn "You must play a matching +2/+4 card OR type 'draw'."
            else putStrLn "Card does not match color or value."

          putStrLn "\nPress Enter to try again..."
          _ <- getLine
          interactionLoop st
        Right nextSt -> do
          runGame nextSt

main :: IO ()
main = do
  st <- setupGame
  runGame st