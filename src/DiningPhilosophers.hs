{-# LANGUAGE TypeApplications #-}

module DiningPhilosophers
  (
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Monad
import System.Random

newtype Fork = Fork {getFork :: MVar ()}

data Table = Table [Fork]

philosopher :: Table -> Int -> IO ()
philosopher (Table forks) idx = forever $ do
  sleep
  liftFork idx
  liftFork (idx + 1)
  putStrLn $ "Philosopher " <> show idx <> " is eating!"
  putDownFork idx
  putDownFork (idx + 1)
  where
    sleep = randomRIO @Int (1000000, 2000000) >>= threadDelay
    liftFork n = takeMVar . getFork . (!! (n `mod` length forks)) $ forks
    putDownFork n = flip putMVar () . getFork . (!! (n `mod` length forks)) $ forks

run :: Int -> IO ()
run n = do
  table <- fmap Table $ replicateM n (Fork <$> newMVar ())
  ps <- mapM (async . philosopher table) [1 .. n]
  threadDelay 10000000
  mapM_ cancel ps
