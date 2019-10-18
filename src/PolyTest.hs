{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module PolyTest
  (
  )
where

import Data.IORef
import qualified Data.Map.Strict as M
import GHC.Generics
import Polysemy

data Config
  = Config
      { host :: String,
        port :: Int
      }
  deriving (Show)

deriving instance Generic Config

data GetConfig m a where
  GetConfig :: GetConfig m Config

makeSem ''GetConfig

data Database idx v m a where
  GetDB :: idx -> Database idx v m (Maybe v)
  PutDB :: idx -> v -> Database idx v m ()

makeSem ''Database

data Command idx v = Get idx | Put idx v | Quit

data CommandParser idx v m a where
  ParseCommand :: String -> CommandParser idx v m (Command idx v)

makeSem ''CommandParser

data TTY m a where
  ReadLine :: TTY m String
  PrintLine :: String -> TTY m ()

makeSem ''TTY

prog :: Members '[Database String String, CommandParser String String, TTY] r => Sem r ()
prog = do
  inp <- readLine
  cmd <- parseCommand @String @String inp
  case cmd of
    Get idx -> getDB idx >>= traverse printLine >> prog
    Put idx v -> putDB idx v >> prog
    Quit -> return ()

runTTY :: Member (Embed IO) r => Sem (TTY ': r) a -> Sem r a
runTTY = interpret $ \case
  ReadLine -> embed $ getLine
  PrintLine s -> embed $ putStrLn s

runDB :: forall idx v r a. (Ord idx, Member (Embed IO) r) => IORef (M.Map idx v) -> Sem (Database idx v ': r) a -> Sem r a
runDB var = interpret $ \case
  GetDB idx -> embed . fmap (M.lookup idx) $ readIORef var
  PutDB idx v -> embed $ do
    db <- readIORef var
    let db' = M.insert idx v db
    writeIORef var db'

-- No error checking!
runCommandParser :: Sem (CommandParser String String ': r) a -> Sem r a
runCommandParser = interpret $ \case
  ParseCommand c -> return cmd
    where
      w = words c
      cmd = case head w of
        "Get" -> Get (w !! 1)
        "Put" -> Put (w !! 1) (w !! 2)
        "Quit" -> Quit

runProg :: IO ()
runProg = do
  v <- newIORef (M.empty @String @String)
  runM . runTTY . runDB v . runCommandParser $ prog
