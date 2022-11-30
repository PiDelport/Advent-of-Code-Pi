{-# LANGUAGE NoMonomorphismRestriction #-}

module Main
  ( main
  ) where

import qualified AoC2021.Day1SonarSweep
import qualified AoC2021.Day2Dive

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
  putStrLn "🎄️ Success!"

test2021 :: IO ()
test2021 = do
  check 1 AoC2021.Day1SonarSweep.part1  7   1400
  check 1 AoC2021.Day1SonarSweep.part2A 5   1429
  check 1 AoC2021.Day1SonarSweep.part2B 5   1429

  check 2 AoC2021.Day2Dive.part1        150 1990000
  where check = checkPart 2021

-- Check one part of day's puzzle against the example and real input.
checkPart
  :: (Read a, Eq b, Show b)
  => InputYear
  -> InputDay
  -> ([a] -> b)
  -> b
  -> b
  -> IO ()
checkPart year day f exampleResult inputResult = do
  checkCase f (inputBase ++ "example") exampleResult
  checkCase f (inputBase ++ "input")   inputResult
  where inputBase = show year ++ "/day-" ++ show day ++ "/"

-- Check one test case.
checkCase :: (Read a, Eq b, Show b) => ([a] -> b) -> InputName -> b -> IO ()
checkCase f inputName expected = do
  input <- readInput inputName
  let result = f input
  let message =
        "💥 check "
          ++ show inputName
          ++ " failed: expected "
          ++ show expected
          ++ ", got "
          ++ show result
  unless (result == expected) (throwIO $ AssertionFailed message)

-- Read an input file, parsing each line.
readInput :: Read a => InputName -> IO [a]
readInput inputName =
  map read . lines <$> readFile ("../aoc-input-files/" ++ inputName ++ ".txt")
