{-# LANGUAGE LambdaCase #-}
module AoC2022.Day2RockPaperScissors
  ( part1
  ) where
import           Data.Maybe                     ( fromJust )

-- A player's choice of shape for a given round.
data Shape = Rock | Paper | Scissors
  deriving (Show, Eq)

-- A round consisting of the opponent's shape, and our own.
type Round = (Shape, Shape)

-- The possible outcomes of a round.
data Outcome = Loss | Draw | Win
  deriving Show

type Score = Int

-- The input describes a list of rounds.
-- >>> parseInput "A Y"
-- Just [(Rock,Paper)]
parseInput :: String -> Maybe [Round]
parseInput = traverse parseRound . lines
 where
  parseRound line = case words line of
    [o, p] -> (,) <$> parseOpponent o <*> parsePlayer p
    _      -> Nothing
  parseOpponent = \case
    "A" -> Just Rock
    "B" -> Just Paper
    "C" -> Just Scissors
    _   -> Nothing
  parsePlayer = \case
    "X" -> Just Rock
    "Y" -> Just Paper
    "Z" -> Just Scissors
    _   -> Nothing

roundOutcome :: Round -> Outcome
roundOutcome = \case
  -- Winning combinations
  (Scissors, Rock    ) -> Win
  (Paper   , Scissors) -> Win
  (Rock    , Paper   ) -> Win
  -- Same shapes are a draw
  (o, p) | o == p      -> Draw
  -- Anything else is a loss
  _otherwise           -> Loss

-- Scoring functions

scoreShape :: Shape -> Score
scoreShape = \case
  Rock     -> 1
  Paper    -> 2
  Scissors -> 3

scoreOutcome :: Outcome -> Score
scoreOutcome = \case
  Loss -> 0
  Draw -> 3
  Win  -> 6

-- >>> scoreRound (Rock, Paper)
-- 8
-- >>> scoreRound (Paper, Rock)
-- 1
-- >>> scoreRound (Scissors, Scissors)
-- 6
scoreRound :: Round -> Score
scoreRound r@(_, p) = scoreShape p + scoreOutcome (roundOutcome r)

part1 :: String -> Score
part1 = sum . map scoreRound . fromJust . parseInput
