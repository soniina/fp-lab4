module DotGenerator (generateDot) where

import DSL

generateDot :: GameMachine -> String
generateDot machine =
  unlines $
    [ "digraph UnoFSM {",
      "    bgcolor=\"transparent\";",
      "    rankdir=TB;",
      "    node [shape=Mrecord, style=filled, fillcolor=\"#DDA0DD\"];",
      "    edge [fontsize=10, color=\"#333333\"];",
      "",
      "    start [shape=circle, style=filled, fillcolor=black, label=\"\", width=0.3];",
      "    start -> CheckPenalty;",
      ""
    ]
      ++ map transitionToDot machine
      ++ ["}"]

transitionToDot :: Transition -> String
transitionToDot t =
  let src = show (fromState t)
      dst = show (toState t)
      lbl = description t

      colorAttr = if src == dst then "color=blue" else ""
   in "    "
        ++ src
        ++ " -> "
        ++ dst
        ++ " [label=\""
        ++ lbl
        ++ "\", "
        ++ colorAttr
        ++ "];"
