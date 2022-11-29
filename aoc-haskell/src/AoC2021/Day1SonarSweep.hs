module AoC2021.Day1SonarSweep (part1) where

part1 :: [Int] -> Int
part1 = countIncreases

-- >>> deltas [1,2,4,2,3]
-- [1,2,-2,1]
deltas :: Num a => [a] -> [a]
deltas ns = zipWith subtract ns (tail ns)

-- >>> countIncreases [1,2,3,2,3]
-- 3
countIncreases :: [Int] -> Int
countIncreases = length . filter (0 <) . deltas
