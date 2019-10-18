{-# LANGUAGE TypeApplications #-}

module AsyncTest where

import Control.Exception
import Control.Concurrent.Async

test :: IO ()
test = withAsync throwsEx $ \async ->
                             wait async

throwsEx :: IO ()
throwsEx = do
  putStrLn "Running code"
  throwIO $ SomeException $ userError "BOOM"

catchAll :: IO a -> IO (Either SomeException a)
catchAll action = withAsync catchAction $ \async -> wait async
  where
    catchAction = try @SomeException  action
