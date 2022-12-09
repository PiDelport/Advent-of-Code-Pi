module AoC2022.Day3RucksackReorganization
    ( part1
    ) where

import           Data.List                      ( elemIndex
                                                , intersect
                                                , nub
                                                )
import           Data.Maybe                     ( fromJust )

type Item = Char

-- A rucksack has two compartments, each the same size.
-- TODO: Array or Vector?
type Rucksack = ([Item], [Item])

-- Each input

-- >>> parseRucksack "vJrwpWtwJgWrhcsFMMfFFhFp"
-- ("vJrwpWtwJgWr","hcsFMMfFFhFp")
parseRucksack :: String -> Maybe Rucksack
parseRucksack s = case length s of
    n | even n    -> Just $ splitAt (n `div` 2) s
      | otherwise -> Nothing


parseInput :: String -> Maybe [Rucksack]
parseInput = traverse parseRucksack . lines

-- Find a rucksack's duplicate item type, assuming there's exactly one.
--
-- Note: each compartment may still contain more than one item of the duplicated type.
--
-- >>> findDuplicateItem ("vJrwpWtwJgWr","hcsFMMfFFhFp")
-- Right 'p'
-- >>> findDuplicateItem ("jqHRNqRjqzjGDLGL", "rsFMfFZSrLrFZsSL")
-- Right 'L'
findDuplicateItem :: Rucksack -> Either (Rucksack, [Item]) Item
findDuplicateItem (xs, ys) = case nub $ xs `intersect` ys of
    [single] -> Right single
    others   -> Left ((xs, ys), others)

-- >>> traverse itemPriority "abcxyzABCXYZ"
-- Just [1,2,3,24,25,26,27,28,29,50,51,52]
itemPriority :: Item -> Maybe Int
itemPriority item = (1 +) <$> elemIndex item (['a' .. 'z'] ++ ['A' .. 'Z'])

part1 :: String -> Int
part1 =
    sum
        . fromJust
        . traverse (itemPriority . fromRight' . findDuplicateItem)
        . fromJust
        . parseInput

-- *  Appendix

fromRight' :: Show a => Either a c -> c
fromRight' = either (error . show) id
