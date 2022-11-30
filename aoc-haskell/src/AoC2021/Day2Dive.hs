-- https://adventofcode.com/2021/day/2

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module AoC2021.Day2Dive
  ( part1
  ) where

import           Data.Foldable                  ( foldl' )
import           Text.Read                      ( readMaybe )

-- Let's use newtypes for the depth and horizontal position:
-- this will give us type errors if we accidentally mix them up.

newtype Depth = Depth Int deriving (Num, Show)
newtype HPos = HPos Int deriving (Num, Show)

data Command
  = Forward HPos
  | Down Depth
  | Up Depth
 deriving (Show)

type Position = (HPos, Depth)

-- Parse a command.
--
--- >>> traverse parseCommand ["forward 5", "down 2", "up 3"]
-- Just [Forward (HPos 5),Down (Depth 2),Up (Depth 3)]
parseCommand :: String -> Maybe Command
parseCommand s = case words s of
  ["forward", n] -> Forward . HPos <$> readMaybe n
  ["down"   , n] -> Down . Depth <$> readMaybe n
  ["up"     , n] -> Up . Depth <$> readMaybe n
  _              -> Nothing

instance Read Command where
  readsPrec _ s = [ (c, "") | Just c <- [parseCommand s] ]

-- Evaluate one command on the state.
--
-- >>> step (10, 0) <$> [Forward 5,Down 2,Up 3]
-- [(Depth 10,HPos 5),(Depth 12,HPos 0),(Depth 7,HPos 0)]
step :: Position -> Command -> Position
step (h, d) c = case c of
  Forward h' -> (h + h', d)
  Down    d' -> (h, d + d')
  Up      d' -> (h, d - d')

-- Calculate the final state by stepping the commands from the initial state.
part1' :: [Command] -> Position
part1' = foldl' step (0, 0)

part1 :: [Command] -> Int
part1 = answer . part1'

answer :: Position -> Int
answer (HPos h, Depth d) = d * h
