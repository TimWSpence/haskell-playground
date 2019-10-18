module STTtest where

import Data.STRef
import Data.Foldable
import Control.Monad.ST

test :: ST s Int
test = do
  var <- newSTRef 0
  traverse_ (const $ modifySTRef var (+1)) [1..100]
  readSTRef var
