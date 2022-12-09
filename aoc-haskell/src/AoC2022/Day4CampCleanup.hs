module AoC2022.Day4CampCleanup
    ( part1
    ) where

import           Data.Maybe                     ( fromJust )
import           Text.Read                      ( readMaybe )

import           AoC2022.Commons                ( spans )

type Assignment = (Int, Int)

type AssignmentPair = (Assignment, Assignment)

-- >>> parseAssignment "2-4"
-- Just (2,4)
parseAssignment :: String -> Maybe Assignment
parseAssignment s = case traverse readMaybe $ spans ('-' /=) s of
    Just [a, b] | a <= b -> Just (a, b)
    _otherwise           -> Nothing

-- >>> parseAssignmentPair "2-4,6-8"
-- Just ((2,4),(6,8))
parseAssignmentPair :: String -> Maybe AssignmentPair
parseAssignmentPair s = case traverse parseAssignment $ spans (',' /=) s of
    Just [a, b] -> Just (a, b)
    _otherwise  -> Nothing

parseInput :: String -> Maybe [AssignmentPair]
parseInput = traverse parseAssignmentPair . lines

-- >>> (2,8) `fullyContains` (3,7)
-- True
fullyContains :: Assignment -> Assignment -> Bool
(a, d) `fullyContains` (b, c) = a <= b && c <= d

eitherFullyContains :: AssignmentPair -> Bool
eitherFullyContains (a1, a2) = a1 `fullyContains` a2 || a2 `fullyContains` a1

part1 :: String -> Int
part1 = length . filter eitherFullyContains . fromJust . parseInput
