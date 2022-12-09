module AoC2022.Day5SupplyStacks
  ( part1
  ) where

import           Data.Char                      ( isDigit
                                                , isSpace
                                                )
import           Data.List                      ( transpose )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , listToMaybe
                                                )
import           Text.Read                      ( readMaybe )

import           AoC2022.Commons                ( paragraphs )
import           Data.Foldable                  ( foldl' )

type Crate = Char

type Stack = [Crate]
type Stacks = [Stack]

type Count = Int
type StackIndex = Int

data Step = Move Count StackIndex StackIndex
  deriving Show

-- Parsing the crate stack diagram is annoying.
--
-- First, reshape the input by reversing and transposing the input,
-- and looking for the labels: this extracts each stack as a list,
-- with the label and bottom crate first.
--
-- >>> example = ["    [D]    ", "[N] [C]    ", "[Z] [M] [P]", " 1   2   3 "]
-- >>> reshape example
-- ["1ZN ","2MCD","3P  "]
--
-- Then, drop the label and whitespace, and reverse the stack,
-- so that the crates are top-first
--
-- >>> parseStacks example
-- ["NZ","DCM","P"]
--
-- We also need to list the tops of each stack for output.
--
-- >>> topsOfStacks $ parseStacks example
-- "NDP"


reshape :: [String] -> [String]
reshape = filter (isDigit . head) . transpose . reverse

toStack :: String -> Stack
toStack = dropWhile isSpace . reverse . drop 1

parseStacks :: [String] -> Stacks
parseStacks = map toStack . reshape

topsOfStacks :: Stacks -> [Crate]
topsOfStacks = map (fromMaybe ' ' . listToMaybe)

-- >>> parseStep "move 1 from 2 to 1"
-- Just (Move 1 2 1)
parseStep :: String -> Maybe Step
parseStep s = case words s of
  ["move", count, "from", fromIndex, "to", toIndex] ->
    Move <$> readMaybe count <*> readMaybe fromIndex <*> readMaybe toIndex
  _otherwise -> Nothing

parseInput :: String -> Maybe (Stacks, [Step])
parseInput s = case paragraphs s of
  [diagram, steps] -> (,) (parseStacks diagram) <$> traverse parseStep steps
  _otherwise       -> Nothing

--
--
-- >>> modifyStack (splitAt 1) 1 ["NZ","DCM","P"]
-- ("D",["NZ","CM","P"])
-- >>> modifyStack (\s -> ((), 'X':s)) 1 ["NZ","DCM","P"]
-- ((),["NZ","XDCM","P"])
modifyStack :: (Stack -> (a, Stack)) -> Int -> Stacks -> (a, Stacks)
modifyStack f i stacks = case splitAt (i - 1) stacks of
  (pre, stack : post) -> case f stack of
    (result, stack') -> (result, pre ++ [stack'] ++ post)
  _otherwise -> error "modifyStack: bad index"

modifyStack_ :: (Stack -> (a, Stack)) -> Int -> Stacks -> Stacks
modifyStack_ f i stacks = case modifyStack f i stacks of
  (_result, stack) -> stack

-- >>> popStack 2 2 ["NZ","DCM","P"]
-- ("DC",["NZ","M","P"])
popStack :: Count -> StackIndex -> Stacks -> ([Crate], Stacks)
popStack count = modifyStack (splitAt count)


-- >>> pushStack "XX" 2 ["NZ","DCM","P"]
-- ((),["NZ","XXDCM","P"])
pushStack :: [Crate] -> Int -> Stacks -> Stacks
pushStack crates = modifyStack_ (\stack -> ((), crates ++ stack))

-- FIXME: Manual state threading
--
-- >>> evalStep ["NZ","DCM","P"] (Move 1 2 1)
-- ["DNZ","CM","P"]
evalStep :: Stacks -> Step -> Stacks
evalStep s0 (Move count fromIndex toIndex) = s2
 where
  (crates, s1) = popStack count fromIndex s0
  s2           = pushStack (reverse crates) toIndex s1

-- FIXME: foldl' with lazy state.
--
-- evalSteps ["NZ","DCM","P"]
evalSteps :: (Stacks, [Step]) -> Stacks
evalSteps (stacks, steps) = foldl' evalStep stacks steps

part1 :: String -> [Crate]
part1 = topsOfStacks . evalSteps . fromJust . parseInput
