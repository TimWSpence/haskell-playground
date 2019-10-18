{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE ScopedTypeVariables #-}



module Lib
  ( someFunc,
  )
where

-- import Polysemy
import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

primes :: Int -> [Int]
primes n = reverse $ go [] [2 .. n]
  where
    go ps [] = ps
    go ps (h : t) = go (h : ps) $ filter ((/= 0) . (`mod` h)) t

caesar :: Int -> String -> String
caesar n = fmap (chr . (+ ord 'a') . (`mod` 26) . (+ n) . (\x -> x - ord 'a') . ord)

cipher :: String -> String -> String
cipher [] msg = msg
cipher sec msg = zipWith shift repeatSec msg
  where
    repeatSec = sec ++ repeatSec
    shift l r = chr . (+ ord 'a') . (`mod` 26) . (+ n) . (\x -> x - ord 'a') . ord $ r
      where
        n = ord l - ord 'a'

shift :: Int -> Char -> Char
shift n =  chr . (+ ord 'a') . (`mod` 26) . (+ n) . (\x -> x - ord 'a') . ord

-- data Console m a where
--   ReadLine :: Console m String
--   PrintLine :: String -> Console m ()

-- makeSem  ''Console

-- data Logging m a where
--   LogInfo :: String -> Logging m ()

-- makeSem ''Logging

-- runConsole :: Member (Embed IO) r => Sem (Console ': r) a -> Sem r a
-- runConsole = interpret $ \case
--   ReadLine -> embed getLine
--   PrintLine s -> embed $ putStrLn s

-- runLogging :: Member (Embed IO) r => Sem (Logging ': r) a -> Sem r a
-- runLogging = interpret $ \case
--   LogInfo s -> embed . putStrLn $ "Info: " <> s

-- prog :: Members '[Console, Logging] r => Sem r ()
-- prog = do
--   i <- readLine
--   logInfo $ "Received input " <> i
--   printLine i
