module Occurrences
  ( occurrences,
  )
where

import Data.List
import qualified Data.Map.Strict as M

-- Count and sort by occurrences in a list
occurrences :: Ord k => [k] -> [(k, Int)]
occurrences = sortOn snd . M.toList . foldr f M.empty
  where
    f k = M.insertWith (+) k 1
