module AoC2022.Day1CalorieCounting
  ( part1
  ) where

import           Control.Applicative            ( Alternative(..) )
import           Data.List                      ( unfoldr )

-- Each elf's inventory is a list of item calories.

type Calories = Int
type Inventory = [Calories]

-- * Parsing

-- We can split a list into continuous spans that satisfy a predicate.

-- >>> spans (0 <) [0,1,2,0,3,4,5,0]
-- [[1,2],[3,4,5]]
spans :: (a -> Bool) -> [a] -> [[a]]
spans p = unfoldr $ guarded (not . null . fst) . span p . dropWhile (not . p)

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p a = if p a then pure a else empty

-- Use this to group and parse input lines into inventories.

-- >>> parseInput $ unlines ["1","2","","3","4","5"]
-- [[1,2],[3,4,5]]
parseInput :: String -> [Inventory]
parseInput = map (map read) . spans (not . null) . lines

part1 :: String -> Calories
part1 = maximum . map sum . parseInput


-- Appendix: Well-known helpers
