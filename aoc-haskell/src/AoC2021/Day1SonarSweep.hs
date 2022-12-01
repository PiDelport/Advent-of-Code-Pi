-- https://adventofcode.com/2021/day/1

module AoC2021.Day1SonarSweep
  ( part1
  , part2A
  , part2B
  ) where

import           Data.List                      ( scanl'
                                                , tails
                                                )

parseInput :: String -> [Int]
parseInput = map read . lines

part1 :: String -> Int
part1 = countIncreases . parseInput

-- >>> deltas [1,2,4,2,3]
-- [1,2,-2,1]
deltas :: Num a => [a] -> [a]
deltas ns = zipWith subtract ns (tail ns)

-- >>> countIncreases [1,2,3,2,3]
-- 3
countIncreases :: [Int] -> Int
countIncreases = length . filter (0 <) . deltas

-- Part 2 adds sliding window sums.

-- We can generate sliding windows directly, and then sum them:

-- >>> slidingWindows 3 [1,2,3,2,3]
-- [[1,2,3],[2,3,2],[3,2,3]]
-- >>> slidingWindows 2 [1,2,3,2,3]
-- [[1,2],[2,3],[3,2],[2,3]]
slidingWindows :: Int -> [a] -> [[a]]
slidingWindows w = takeWhile ((w ==) . length) . map (take w) . tails

-- >>> slidingWindowSums 3 [1,2,3,2,3]
-- [6,7,8]
-- >>> slidingWindowSums 2 [1,2,3,2,3]
-- [3,5,5,5]
slidingWindowSums :: Num a => Int -> [a] -> [a]
slidingWindowSums w = map sum . slidingWindows w

part2A :: String -> Int
part2A = countIncreases . slidingWindowSums 3 . parseInput

-- However, we can be more clever:
--
-- Each sliding window can be derived from its neighbours by
-- adding / removing items at its edges, and the same goes
-- for the sum of each sliding window.
--
-- We can use this to calculate the sliding window sums
-- in a linear, rolling fashion:
--
-- 1. Calculate the initial sum of the first window.
-- 2. Generate pairs corresponding to the edges of each window.
-- 3. Update the rolling sum by adding / subtracting each pair.

-- >>> slidingWindowSums' 3 [1,2,3,2,3]
-- [6,7,8]
-- >>> slidingWindowSums' 2 [1,2,3,2,3]
-- [3,5,5,5]
slidingWindowSums' :: Num a => Int -> [a] -> [a]
slidingWindowSums' w ns = scanl' update (sum firstWindow) edges
 where
  (firstWindow, ns') = splitAt w ns
  edges              = zip ns ns'

  update windowSum (l, r) = windowSum - l + r

part2B :: String -> Int
part2B = countIncreases . slidingWindowSums' 3 . parseInput
