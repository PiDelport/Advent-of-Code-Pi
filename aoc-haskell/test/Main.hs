{-# LANGUAGE NoMonomorphismRestriction #-}

module Main
  ( main
  ) where

import qualified AoC2021.Day1SonarSweep
import qualified AoC2021.Day2Dive

import qualified AoC2022.Day1CalorieCounting
import qualified AoC2022.Day2RockPaperScissors
import qualified AoC2022.Day3RucksackReorganization

import           Control.Exception              ( AssertionFailed(..)
                                                , throwIO
                                                )
import           Control.Monad                  ( unless )

-- Name of an input file, relative to aoc-input-files, without ".txt".
type InputName = String

type InputYear = Int
type InputDay = Int

main :: IO ()
main = do
  test2021
  test2022
  putStrLn "🎄️ Success!"

test2021 :: IO ()
test2021 = do
  check 1 AoC2021.Day1SonarSweep.part1  7   1400
  check 1 AoC2021.Day1SonarSweep.part2A 5   1429
  check 1 AoC2021.Day1SonarSweep.part2B 5   1429

  check 2 AoC2021.Day2Dive.part1        150 1990000
  check 2 AoC2021.Day2Dive.part2        900 1975421260
  where check = checkPart 2021

test2022 :: IO ()
test2022 = do
  check 1 AoC2022.Day1CalorieCounting.part1        24000 68787
  check 1 AoC2022.Day1CalorieCounting.part2        45000 198041

  check 2 AoC2022.Day2RockPaperScissors.part1      15    13052

  check 3 AoC2022.Day3RucksackReorganization.part1 157   7737
  where check = checkPart 2022

-- Check one part of day's puzzle against the example and real input.
checkPart
  :: (Eq a, Show a) => InputYear -> InputDay -> (String -> a) -> a -> a -> IO ()
checkPart year day f exampleResult inputResult = do
  checkCase f (inputBase ++ "example") exampleResult
  checkCase f (inputBase ++ "input")   inputResult
  where inputBase = show year ++ "/day-" ++ show day ++ "/"

-- Check one test case.
checkCase :: (Eq a, Show a) => (String -> a) -> InputName -> a -> IO ()
checkCase f inputName expected = do
  input <- readFile ("../aoc-input-files/" ++ inputName ++ ".txt")
  let result = f input
  let message =
        "💥 check "
          ++ show inputName
          ++ " failed: expected "
          ++ show expected
          ++ ", got "
          ++ show result
  unless (result == expected) (throwIO $ AssertionFailed message)
