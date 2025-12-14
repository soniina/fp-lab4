module Logic (getCardFromHand, getTopCard, isValidMove, playCard, drawCard, acceptPenalty, applySpecialCardEffect, switchTurn) where

import Control.Applicative ((<|>))
import Control.Monad.State
import Types
import Utils (shuffle)

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs

getCardFromHand :: GameState -> Int -> Card
getCardFromHand st index =
  let player = players st !! currentPlayerIndex st
   in hand player !! index

getTopCard :: GameState -> Card
getTopCard st = head (discardPile st)

isValidMove :: GameState -> PlayerAction -> Bool
isValidMove st action =
  case action of
    PlayCard cardId _ ->
      let card = getCardFromHand st cardId
          top = getTopCard st
       in case (card, top) of
            (Card (Just playedColor) _, Card _ Wild) -> Just playedColor == activeColor st
            (Card (Just playedColor) _, Card _ WildDrawFour) -> Just playedColor == activeColor st
            (Card (Just playedColor) playedValue, Card (Just topColor) topValue) -> playedColor == topColor || playedValue == topValue
            _ -> False
    PlayWildCard {} -> True
    _ -> False

updatePlayer :: Int -> (Player -> Player) -> Game ()
updatePlayer pId f = modify $ \st ->
  let allPlayers = players st
      (before, rest) = splitAt pId allPlayers
   in case rest of
        (target : after) -> st {players = before ++ [f target] ++ after}
        [] -> st

playCard :: PlayerAction -> Game ()
playCard action = case action of
  PlayCard cardId _ -> applyPlay cardId Nothing
  PlayWildCard cardId chosenColor _ -> applyPlay cardId (Just chosenColor)
  _ -> return ()
  where
    applyPlay cardId chosenColor = do
      st <- get
      let pId = currentPlayerIndex st
      let player = players st !! pId
      let card = hand player !! cardId

      updatePlayer pId (\p -> p {hand = removeAt cardId (hand p)})
      modify $ \s ->
        s
          { discardPile = card : discardPile s,
            activeColor = color card <|> chosenColor
          }
      checkUno action

checkUno :: PlayerAction -> Game ()
checkUno action = do
  st <- get
  let player = players st !! currentPlayerIndex st

  let saidUno = case action of
        PlayCard _ s -> s
        PlayWildCard _ _ s -> s
        _ -> True

  when
    (length (hand player) == 1 && not saidUno)
    $ do
      drawCard DrawCard
      drawCard DrawCard

acceptPenalty :: PlayerAction -> Game ()
acceptPenalty _ = do
  st <- get
  let penalty = pendingPenalty st
  replicateM_ penalty (drawCard DrawCard)
  modify $ \s -> s {pendingPenalty = 0}

drawCard :: PlayerAction -> Game ()
drawCard _ = do
  st <- get
  case deck st of
    [] -> do
      let discard = discardPile st
      if length discard < 2
        then return ()
        else do
          let (shuffledDeck, newGen) = shuffle (tail discard) $ rndGen st
          modify $ \s ->
            s
              { deck = shuffledDeck,
                discardPile = [head discard],
                rndGen = newGen
              }
          drawCard DrawCard
    (top : rest) -> do
      updatePlayer (currentPlayerIndex st) (\p -> p {hand = top : hand p})
      modify $ \s -> s {deck = rest}

switchTurn :: PlayerAction -> Game ()
switchTurn _ = modify $ \st ->
  let total = length (players st)
      pId = currentPlayerIndex st
      next = (pId + direction st) `mod` total
   in st {currentPlayerIndex = next}

applySpecialCardEffect :: PlayerAction -> Game ()
applySpecialCardEffect _ = do
  st <- get
  let top = getTopCard st

  case value top of
    Reverse ->
      modify $ \s -> s {direction = direction s * (-1)}
    Skip ->
      switchTurn DrawCard
    DrawTwo -> do
      modify $ \s -> s {pendingPenalty = pendingPenalty s + 2}
    WildDrawFour ->
      modify $ \s -> s {pendingPenalty = pendingPenalty s + 4}
    _ -> return ()
