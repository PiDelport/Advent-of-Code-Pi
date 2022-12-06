-- https://adventofcode.com/2021/day/2

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module AoC2021.Day2Dive
  ( part1
  , part2
  ) where

import           Data.Foldable                  ( foldl' )
import           Data.Maybe                     ( fromJust )
import           Text.Read                      ( readMaybe )

-- Let's use newtypes for the depth and horizontal position:
-- this will give us type errors if we accidentally mix them up.

newtype Depth = Depth Int deriving (Num, Show)
newtype HPos = HPos Int deriving (Num, Show)

-- The command values have two different interpretations,
-- so let's parameterize the types of the horizontal and vertical arguments.

data Command' h v
  = Forward h
  | Down v
  | Up v
 deriving (Show)

type Position = (HPos, Depth)

type Command = Command' HPos Depth

-- Parse a command.

parseCommand'
  :: (Read a, Read b) => (a -> h) -> (b -> v) -> String -> Maybe (Command' h v)
parseCommand' h v s = case words s of
  ["forward", n] -> Forward . h <$> readMaybe n
  ["down"   , n] -> Down . v <$> readMaybe n
  ["up"     , n] -> Up . v <$> readMaybe n
  _              -> Nothing

--- >>> traverse parseCommand ["forward 5", "down 2", "up 3"]
-- Just [Forward (HPos 5),Down (Depth 2),Up (Depth 3)]
parseCommand :: String -> Maybe (Command' HPos Depth)
parseCommand = parseCommand' HPos Depth

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

part1 :: String -> Int
part1 = answer . part1' . fromJust . traverse parseCommand . lines

answer :: Position -> Int
answer (HPos h, Depth d) = d * h

-- For Part 2, we also need to keep track of the aim.

newtype Aim = Aim Int deriving (Num, Show)

type State = (Position, Aim)

getPosition :: State -> Position
getPosition (pos, _aim) = pos

-- Instead of interpreting commands relative to the horizontal and vertical axes,
-- we can interpret commands as thrusting along the aim, and adjusting the aim.

newtype Thrust = Thrust Int deriving (Num, Show)

type CommandAimed = Command' Thrust Aim

-- Helper: Calculate the movement vector for a given aim and thrust.
movementVector :: Aim -> Thrust -> Position
movementVector (Aim aim) (Thrust thrust) = (HPos thrust, Depth (aim * thrust))

-- Helper: Vector addition.
(/+/) :: Position -> Position -> Position
(h, d) /+/ (h', d') = (h + h', d + d')

-- >>> stepAimed ((10, 0), 0) <$> [Forward 5,Down 2,Up 3]
-- [((HPos 15,Depth 0),Aim 0),((HPos 10,Depth 0),Aim 2),((HPos 10,Depth 0),Aim (-3))]
stepAimed :: State -> CommandAimed -> State
stepAimed (pos, aim) c = case c of
  Forward thrust   -> (pos /+/ movementVector aim thrust, aim)
  Down    turnDown -> (pos, aim + turnDown)
  Up      turnUp   -> (pos, aim - turnUp)

part2' :: [CommandAimed] -> State
part2' = foldl' stepAimed ((0, 0), 0)

parseCommandAimed :: String -> Maybe CommandAimed
parseCommandAimed = parseCommand' Thrust Aim

part2 :: String -> Int
part2 =
  answer . getPosition . part2' . fromJust . traverse parseCommandAimed . lines
