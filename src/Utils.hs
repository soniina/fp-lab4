module Utils (shuffle) where

import Data.List (sortOn)
import System.Random (StdGen, randoms, split)

shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle list gen =
  let (gen1, gen2) = split gen
      n = length list
      weights = take n (randoms gen1 :: [Int])
      zipped = zip weights list
      sorted = sortOn fst zipped
   in (map snd sorted, gen2)
