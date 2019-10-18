{-# LANGUAGE TypeApplications #-}

module ConduitTest
  (
  )
where

import Conduit
import Control.Exception
import Control.Monad
import qualified Data.Conduit.Text as CT
import qualified Data.Text.IO as TIO
-- import Data.Conduit - this seems to be re-exported by Conduit but stuff like tryC is hidden there

data StreamError = StreamError String deriving (Show)

instance Exception StreamError

errorTest = runConduitRes @IO $ yieldMany [1, 2, error "boom", 3] .| sinkList

errorTest2 :: IO ()
errorTest2 =
  void . runConduitRes . tryC @_ @SomeException $
    (sourceFileBS "/tmp/test" >> sourceFileBS "/tmp/not_there")
      .| decodeUtf8C
      .| linesUnboundedC
      -- .| CT.lines -- possibly a better option as it also has a bounded variation taking a max length
      .| mapMC (liftIO . TIO.putStrLn)
      .| sinkNull

-- errorTest3 :: IO ()
-- errorTest3 =
--   void . runConduitRes $ (tryC @_ @SomeException
--     (sourceFileBS "/tmp/test" >> sourceFileBS "/tmp/not_there"))
--   .| iterMC (liftIO . print)
--   .| sinkNull

test3 :: IO ()
test3 = runConduitRes $ sourceFileBS "input.txt" .| sinkFile "output.txt"
