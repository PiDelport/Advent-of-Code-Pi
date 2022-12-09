module AoC2022.Day1CalorieCounting
  ( part1
  , part2
  ) where

import           Data.List                      ( sortOn )
import           Data.Ord                       ( Down(..) )

import           AoC2022.Commons                ( paragraphs )

-- Each elf's inventory is a list of item calories.

type Calories = Int
type Inventory = [Calories]

-- * Parsing

-- Group and parse input lines into inventories.

-- >>> parseInput $ unlines ["1","2","","3","4","5"]
-- [[1,2],[3,4,5]]
parseInput :: String -> [Inventory]
parseInput = map (map read) . paragraphs

part1 :: String -> Calories
part1 = maximum . map sum . parseInput

-- For Part 2, we can sort to get the top three.

part2 :: String -> Calories
part2 = sum . take 3 . sortOn Down . map sum . parseInput
