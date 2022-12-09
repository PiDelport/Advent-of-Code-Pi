module AoC2022.Commons
    ( paragraphs
    , spans
    ) where

import           Control.Applicative            ( Alternative(..) )
import           Data.List                      ( unfoldr )

-- Split a list into continuous spans that satisfy a predicate.
--
-- >>> spans (0 <) [0,1,2,0,3,4,5,0]
-- [[1,2],[3,4,5]]
spans :: (a -> Bool) -> [a] -> [[a]]
spans p = unfoldr $ guarded (not . null . fst) . span p . dropWhile (not . p)

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p a = if p a then pure a else empty

-- Break a string into groups of non-empty lines separated by empty lines.
--
-- >>> paragraphs $ unlines ["1","2","","3","4","5"]
-- [["1","2"],["3","4","5"]]
paragraphs :: String -> [[String]]
paragraphs = spans (not . null) . lines
