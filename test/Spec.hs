import Control.Monad.State
import DSL
import Logic
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit
import Types
import UnoRules

testState :: [Card] -> [Card] -> GameState
testState p1Hand topCard =
  GameState
    { players = [Player "Alice" p1Hand, Player "Bob" [blue9, blue9]],
      deck = [green1, green2],
      discardPile = topCard,
      activeColor = color (head topCard),
      currentPlayerIndex = 0,
      direction = 1,
      currentPhase = WaitForInput,
      pendingPenalty = 0,
      rndGen = mkStdGen 12345
    }

red5, red3, blue5, blue9, green1, green2, yellowSkip, blueRev, redDraw2, wild, wild4 :: Card
red5 = Card (Just Red) (Number 5)
red3 = Card (Just Red) (Number 3)
blue5 = Card (Just Blue) (Number 5)
blue9 = Card (Just Blue) (Number 9)
green1 = Card (Just Green) (Number 1)
green2 = Card (Just Green) (Number 2)
yellowSkip = Card (Just Yellow) Skip
blueRev = Card (Just Blue) Reverse
redDraw2 = Card (Just Red) DrawTwo
wild = Card Nothing Wild
wild4 = Card Nothing WildDrawFour

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Uno Tests"
    [ validMovesTests,
      coreMechanicsTests,
      specialEffectsTests,
      unoShoutLogicTests,
      penaltyStackingTests,
      fsmTransitionsTests,
      reshuffleTests
    ]

validMovesTests :: TestTree
validMovesTests =
  testGroup
    "Card Validation"
    [ testCase "Valid: Same Color" $ do
        let st = testState [red5] [red3]
        isValidMove st (PlayCard 0 False) @?= True,
      testCase "Valid: Same Value" $ do
        let st = testState [red5] [blue5]
        isValidMove st (PlayCard 0 False) @?= True,
      testCase "Invalid: Different Color and Value" $ do
        let st = testState [red5] [blue9]
        isValidMove st (PlayCard 0 False) @?= False,
      testCase "Valid: Wild on anything" $ do
        let st = testState [wild] [blue9]
        isValidMove st (PlayWildCard 0 Red False) @?= True,
      testCase "Valid: Color matches Active Color (on top of Wild)" $ do
        let st = (testState [red5] [wild]) {activeColor = Just Red}
        isValidMove st (PlayCard 0 False) @?= True,
      testCase "Invalid: Color mismatches Active Color (on top of Wild)" $ do
        let st = (testState [blue5] [wild]) {activeColor = Just Red}
        isValidMove st (PlayCard 0 False) @?= False
    ]

coreMechanicsTests :: TestTree
coreMechanicsTests =
  testGroup
    "Core Mechanics (Draw & Play)"
    [ testCase "PlayCard removes card and updates discard" $ do
        let st = testState [red5, blue5] [red3]
        let endSt = execState (playCard (PlayCard 0 True)) st

        length (hand $ head $ players endSt) @?= 1
        head (discardPile endSt) @?= red5
        activeColor endSt @?= Just Red,
      testCase "DrawCard takes card from deck" $ do
        let st = testState [red5] [red3]
        let endSt = execState (drawCard DrawCard) st

        length (hand $ head $ players endSt) @?= 2
        length (deck endSt) @?= 1,
      testCase "SwitchTurn wraps around" $ do
        let st = testState [] []
        let st1 = execState (switchTurn DrawCard) st
        currentPlayerIndex st1 @?= 1

        let st2 = execState (switchTurn DrawCard) st1
        currentPlayerIndex st2 @?= 0
    ]

specialEffectsTests :: TestTree
specialEffectsTests =
  testGroup
    "Special Card Effects"
    [ testCase "Reverse flips direction" $ do
        let st = testState [] [blueRev]
        let endSt = execState (applySpecialCardEffect DrawCard) st
        direction endSt @?= (-1),
      testCase "DrawTwo adds penalty" $ do
        let st = testState [] [redDraw2]
        let endSt = execState (applySpecialCardEffect DrawCard) st
        pendingPenalty endSt @?= 2,
      testCase "WildDrawFour adds penalty +4" $ do
        let st = testState [] [wild4]
        let endSt = execState (applySpecialCardEffect DrawCard) st
        pendingPenalty endSt @?= 4,
      testCase "Skip calls switchTurn" $ do
        let st = testState [] [yellowSkip]
        let endSt = execState (applySpecialCardEffect DrawCard) st
        currentPlayerIndex endSt @?= 1
    ]

unoShoutLogicTests :: TestTree
unoShoutLogicTests =
  testGroup
    "Uno Shout Logic"
    [ testCase "PENALTY: Forgot to say Uno (2 -> 1 card)" $ do
        let st = testState [red5, blue5] [red3]
        let endSt = execState (playCard (PlayCard 0 False)) st

        length (hand $ head $ players endSt) @?= 3,
      testCase "NO PENALTY: Said Uno correctly" $ do
        let st = testState [red5, blue5] [red3]
        let endSt = execState (playCard (PlayCard 0 True)) st

        length (hand $ head $ players endSt) @?= 1,
      testCase "NO PENALTY: Victory (1 -> 0 cards)" $ do
        let st = testState [red5] [red3]
        let endSt = execState (playCard (PlayCard 0 False)) st

        length (hand $ head $ players endSt) @?= 0
    ]

penaltyStackingTests :: TestTree
penaltyStackingTests =
  testGroup
    "Penalty Stacking & Accepting"
    [ testCase "Accept Penalty: Draws cards and clears penalty" $ do
        let st = (testState [] []) {pendingPenalty = 2}
        let endSt = execState (acceptPenalty DrawCard) st

        pendingPenalty endSt @?= 0
        length (hand $ head $ players endSt) @?= 2,
      testCase "FSM: Can Stack +2 on +2" $ do
        let st = (testState [redDraw2] [redDraw2]) {currentPhase = CheckPenalty, pendingPenalty = 2}

        let result = step unoMachine st (PlayCard 0 False)

        case result of
          Left _ -> assertFailure "Should allow stacking +2 on +2"
          Right newSt -> currentPhase newSt @?= ExecuteCard,
      testCase "FSM: Can Stack +4 on +4" $ do
        let st = (testState [wild4] [wild4]) {currentPhase = CheckPenalty, pendingPenalty = 4}
        let result = step unoMachine st (PlayWildCard 0 Red False)
        case result of
          Left _ -> assertFailure "Should allow stacking +4 on +4"
          Right newSt -> currentPhase newSt @?= ExecuteCard,
      testCase "FSM: Cannot play normal card when Penalty active" $ do
        let st = (testState [red5] [redDraw2]) {currentPhase = CheckPenalty, pendingPenalty = 2}
        let result = step unoMachine st (PlayCard 0 False)
        case result of
          Left _ -> return ()
          Right _ -> assertFailure "Should NOT allow normal card during penalty"
    ]

fsmTransitionsTests :: TestTree
fsmTransitionsTests =
  testGroup
    "FSM State Transitions"
    [ testCase "CheckPenalty -> ExecuteCard (Stack +2)" $ do
        let st = (testState [redDraw2] [redDraw2]) {currentPhase = CheckPenalty, pendingPenalty = 2}
        let res = step unoMachine st (PlayCard 0 False)
        case res of
          Right s -> currentPhase s @?= ExecuteCard
          Left e -> assertFailure e,
      testCase "CheckPenalty -> SwitchTurn (Accept)" $ do
        let st = (testState [] []) {currentPhase = CheckPenalty, pendingPenalty = 2}
        let res = step unoMachine st DrawCard
        case res of
          Right s -> currentPhase s @?= SwitchTurn
          Left e -> assertFailure e,
      testCase "CheckPenalty -> WaitForInput (Penalty is 0)" $ do
        let st = (testState [] []) {currentPhase = CheckPenalty, pendingPenalty = 0}
        let res = step unoMachine st DrawCard
        case res of
          Right s -> currentPhase s @?= WaitForInput
          Left e -> assertFailure e,
      testCase "WaitForInput -> ExecuteCard (Valid Move)" $ do
        let st = testState [red5] [red3]
        let res = step unoMachine st (PlayCard 0 False)
        case res of
          Right s -> currentPhase s @?= ExecuteCard
          Left e -> assertFailure e,
      testCase "WaitForInput -> SwitchTurn (Draw)" $ do
        let st = testState [red5] [red3]
        let res = step unoMachine st DrawCard
        case res of
          Right s -> currentPhase s @?= SwitchTurn
          Left e -> assertFailure e,
      testCase "ExecuteCard -> GameOver (Hand Empty)" $ do
        let st = (testState [] []) {currentPhase = ExecuteCard}
        let res = step unoMachine st DrawCard
        case res of
          Right s -> currentPhase s @?= GameOver
          Left e -> assertFailure e,
      testCase "ExecuteCard -> ApplyEffect (Hand Not Empty)" $ do
        let st = (testState [red5] [blue5]) {currentPhase = ExecuteCard}
        let res = step unoMachine st DrawCard
        case res of
          Right s -> currentPhase s @?= ApplyEffect
          Left e -> assertFailure e,
      testCase "ApplyEffect -> SwitchTurn" $ do
        let st = (testState [] []) {currentPhase = ApplyEffect}
        let res = step unoMachine st DrawCard
        case res of
          Right s -> currentPhase s @?= SwitchTurn
          Left e -> assertFailure e,
      testCase "SwitchTurn -> CheckPenalty (Next Player)" $ do
        let st = (testState [] []) {currentPhase = SwitchTurn}
        let res = step unoMachine st DrawCard
        case res of
          Right s -> currentPhase s @?= CheckPenalty
          Left e -> assertFailure e
    ]

reshuffleTests :: TestTree
reshuffleTests =
  testGroup
    "Deck Reshuffle"
    [ testCase "Deck refills from discard when empty" $ do
        let discard = [red5, blue5, red3, blue9, green1]
        let st = (testState [] []) {deck = [], discardPile = discard}

        let endSt = execState (drawCard DrawCard) st

        length (deck endSt) @?= 3
        length (discardPile endSt) @?= 1
    ]