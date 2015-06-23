-- Copyright © 2015 Bart Massey
-- [This program is licensed under the "3-clause ('new') BSD License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- Simulation of nontransitive dice

import Control.Monad.Random
import System.Environment (getArgs)
import Text.Printf (printf)

-- | A die is a list of 6 face values.
type Die = [Int]
-- | A pair of dice is a two-tuple.
type Dice = (Die, Die)

-- | Roll each die and return True if d1 beats d2
experiment :: Dice -> Rand StdGen Bool
experiment (d1, d2) = do
  -- Get a random face on each die.
  i1 <- getRandomR (1, length d1)
  i2 <- getRandomR (1, length d2)
  -- Get the value on that face.
  let v1 = d1 !! (i1 - 1)
  let v2 = d2 !! (i2 - 1)
  -- Compare face values.
  return (v1 > v2)

-- | Run a sequence of n experiments with the given dice.
-- Return the count of first-die wins.
runExperiments :: Int -> Dice -> Rand StdGen Int
-- No experiments gives no wins.
runExperiments 0 _ = return 0
runExperiments n dice = do
  -- Run a single experiment.
  ok <- experiment dice
  -- Score 1 point for a d1 win, 0 points for a d2 win or draw.
  let cur = case ok of
              True -> 1
              False -> 0
  -- Get the score from running the rest of the experiments.
  rest <- runExperiments (n - 1) dice
  -- Return the modified score.
  return (cur + rest)

-- | Run a bunch of experiments with stay or switch as indicated
-- on the command line.
main :: IO ()
main = do
  -- The usage message.
  let usage = "usage: <count>"
  -- Get the list of arguments from the command line.
  args <- getArgs
  -- The argument should be a count.
  let countStr = case args of
                 [cs] -> cs
                 _ -> error usage
  -- Try to convert the count to an integer.
  let n = case reads countStr of
            [(count, "")] -> count
            _ -> error ("bad count: " ++ usage)
  -- Read the dice.
  diceFile <- getContents
  let diceList = map (map read . words) $ lines diceFile
  let dice = case diceList of
             [l1, l2] | length l1 == 6 && length l2 == 6 -> (l1, l2)
             _ -> error "bad dice"
  -- Make a random number generator and run the experiments with it.
  result <- evalRandIO (runExperiments n dice)
  -- Find the percentage of wins.
  let percent = 100.0 * fromIntegral result / fromIntegral n
  -- Show how many times the player won a car, and the percentage.
  printf "%d / %d (%2.0f%%)\n" result n (percent :: Double)
