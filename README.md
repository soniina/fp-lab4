# Лабораторная работа №4

### **Павличенко Софья P3315**

## Требования к разработанному ПО

Разработать eDSL (embedded Domain Specific Language) для описания конечных автоматов. Язык должен позволять в явном виде описывать переходы между состояниями, условия переходов и побочные эффекты и быть запускаемым и генерирующим описание в формате dot. На базе разработанного eDSL реализовать модель карточной игры Uno.


#### Описание алгоритма:
Игра моделируется как конечный автомат со следующими состояниями:
1.  **CheckPenalty:** Проверка, наложены ли на игрока штрафы (карты +2/+4). Игрок может либо принять штраф, либо «перевести» его дальше.
2.  **WaitForInput:** Ожидание хода игрока (сыграть карту или взять из колоды).
3.  **ExecuteCard:** Применение хода (перемещение карты из руки в сброс). На этом этапе также происходит проверка условия "Uno" (осталась 1 карта).
4.  **CheckVictory:** Проверка наступления победы (пустая рука).
5.  **ApplyEffect:** Применение спецэффектов карты (Skip, Reverse, +2, +4).
6.  **SwitchTurn:** Передача хода следующему игроку.ма
7.  **GameOver:** Завершающее состояние при победе.

Автомат работает циклически. Входными данными для переходов являются действия игрока (`PlayerAction`). Движок игры (`step`) ищет подходящий переход в списке правил и, если условия выполнены, изменяет состояние игры (`GameState`) через монаду `State`.


## Реализация

Программа разделена на модули:


**`Types.hs`**<br>
Определяет базовые типы данных с использованием алгебраических типов данных: масти и номиналы карт, фазы игры, возможные действия игрока и структуру глобального состояния (`GameState`), хранящую колоду, игроков и текущие флаги.
```haskell
data GamePhase
  = CheckPenalty
  | WaitForInput
  | ExecuteCard
  | ApplyEffect
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
    rndGen :: StdGen
  }
  deriving (Show)

type Game a = State GameState a
```

**`DSL.hs` — Ядро движка FSM**<br>
Описывает абстракцию конечного автомата. Определяет тип `Transition`, связывающий исходное и целевое состояния с условием и эффектом. Функция `step` выступает интерпретатором: она принимает список переходов и текущее состояние, находит валидный переход и применяет изменения.

```haskell
module DSL where
import Types
import Data.List (find)
import Control.Monad.State (execState)

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
```

**`UnoRules.hs` — Декларативное описание игры**<br>
Связывает движок DSL и логику. Здесь `unoMachine` определяется как список переходов, что позволяет наглядно видеть структуру игры и менять правила, просто добавляя элементы в список.

```haskell
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

unoMachine :: GameMachine
unoMachine =
  [ stackPenaltyMove, acceptPenaltyMove, noPenaltyMove
  , validateMove, drawMove
  , victoryMove, noVictoryMove, effectsDoneMove, switchTurnMove
  ]
```

**`Logic.hs` — Игровая логика**<br>
Реализует правила игры Uno и манипуляции с данными через монаду `State`. Содержит функции для проверки валидности хода, перемешивания колоды, раздачи карт и обработки спецэффектов (+2, Skip). Здесь реализуется "императивная" часть логики внутри чистого контекста.

```haskell
playCard :: PlayerAction -> Game ()
playCard action = case action of
  PlayCard cardId _ -> applyPlay cardId Nothing
  PlayWildCard cardId chosenColor _ -> applyPlay cardId (Just chosenColor)
  _ -> return ()
  where
    applyPlay cardId chosenColor = do
      st <- get
      -- ... логика удаления карты из руки ...
      updatePlayer pId (\p -> p {hand = removeAt cardId (hand p)})
      modify $ \s -> s { discardPile = card : discardPile s, ... }
      
      -- Проверка на "Uno!" 
      checkUno action

checkUno :: PlayerAction -> Game ()
checkUno action = do
  st <- get
  let player = players st !! currentPlayerIndex st
  -- Проверка флага saidUno из action
  let saidUno = ... 
  when
    (length (hand player) == 1 && not saidUno)
    $ do
      drawCard DrawCard -- Штраф
      drawCard DrawCard

```

**`DotGenerator.hs` — Визуализация**<br>
Транслирует список переходов `unoMachine` в текстовый формат DOT (Graphviz). Это позволяет автоматически генерировать актуальную схему автомата при каждом запуске программы, что упрощает отладку и документирование логики.

**`Utils.hs` — Вспомогательные функции**<br>
Содержит простую функцию тасования списка с использованием чистого генератора случайных чисел `System.Random`.
```haskell
shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle list gen =
  let (gen1, gen2) = split gen
      n = length list
      weights = take n (randoms gen1 :: [Int])
      zipped = zip weights list
      sorted = sortOn fst zipped
   in (map snd sorted, gen2)
```

**`Main.hs` — Точка входа и UI**<br>
Отвечает за функции ввода-вывода (IO). Реализует игровой цикл, инициализацию игры, парсинг текстовых команд пользователя и рекурсивную обработку автоматических и интерактивных фаз автомата.

```haskell
setupGame :: IO GameState
setupGame = do
  putStrLn "=== WELCOME TO UNO ==="

  count <- askPlayerCount
  names <- askPlayerNames count

  putStrLn "\nShuffling deck..."
  gen <- newStdGen
  let (shuffledDeck, newGen) = shuffle generateDeck gen

  let (playersList, deckRest) = distributeCards names shuffledDeck

  let (topCard : finalDeck) = deckRest

  return $
    GameState
      { players = playersList,
        deck = finalDeck,
        discardPile = [topCard],
        activeColor = color topCard <|> Just Red,
        currentPlayerIndex = 0,
        direction = 1,
        currentPhase = CheckPenalty,
        pendingPenalty = 0,
        rndGen = newGen
      }

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
        then interactionLoop st
        else do
          let result = step unoMachine st DrawCard
          case result of
            Right nextSt -> runGame nextSt
            Left err -> do
              putStrLn $ "CRITICAL ERROR in Auto-Phase (" ++ show (currentPhase st) ++ "): " ++ err
```




## Ввод/вывод программы

При запуске программа генерирует файл `uno.dot` и предлагает настроить игру.

**Запуск:**
```text
Generating 'uno.dot'...
Done!

=== WELCOME TO UNO ===

Enter number of players (2-10): 2
Enter name for Player 1: Alice
Enter name for Player 2: Bob

Shuffling deck...
```

**Игровой процесс:** Штраф = 0
```text
=== TURN: Alice ===

--- TABLE ---
Top Card: Number 4 🔴 (Red)
Active Color: Red

--- YOUR HAND ---
0: WildDrawFour 🌈 (Wild)
1: Number 3 🟢 (Green)
2: Wild 🌈 (Wild)
3: Number 7 🟢 (Green)
4: Number 0 🟢 (Green)
5: Wild 🌈 (Wild)
6: DrawTwo 🟡 (Yellow)

--- COMMANDS ---
play <id>                   -> Play card (e.g., 'play 0')
play <id> <color>           -> Play Wild card (e.g., 'play 1 red')
play <id> [color] uno       -> Play and shout UNO!
draw                        -> Take a card from deck

> play 0 green
```

**Игровой процесс:** Штраф = 4
```text
=== TURN: Bob ===
⚠️ PENALTY ACTIVE: +4

--- TABLE ---
Top Card: WildDrawFour 🌈 (Wild)
Active Color: Green

--- YOUR HAND ---
0: Number 4 🟡 (Yellow)
1: Reverse 🟡 (Yellow)
2: Number 9 🔴 (Red)
3: Reverse 🔵 (Blue)
4: DrawTwo 🔵 (Blue)
5: DrawTwo 🔴 (Red)
6: Number 2 🔵 (Blue)

--- COMMANDS ---
draw                        -> Accept penalty cards
play <id> [color]           -> Stack penalty (Counter-attack!)

> draw  
```

**Реакция на победу:**
```text
==========================================
🏆 WINNER: Alice 🏆
==========================================
```

**Реакция на ошибку (Невозможный переход):**
```text
> play 0

❌ MOVE REJECTED! Card does not match color or value.
Press Enter to try again...
```

---

## Выводы

Реализация игры Uno через модель конечного автомата оказалась эффективным и интересным архитектурным решением. Представление игровой логики в виде набора состояний и переходов сделало систему предсказуемой и легко расширяемой, а возможность автоматической генерации графа позволила наглядно верифицировать правила и убедиться в отсутствии тупиковых ветвей логики.<br>
Система типов Haskell отлично подошла для формализации предметной области, исключая возможность появления некорректных состояний еще на этапе компиляции. Особую роль в реализации сыграла монада State. Она позволила инкапсулировать изменяемое состояние игры и описывать мутации — раздачу карт, сброс, смену очередности ходов — в привычном и удобном императивном стиле. При этом код остался чистым и безопасным, сохранив все преимущества функционального подхода: предсказуемость, тестируемость и отсутствие неконтролируемых побочных эффектов.

