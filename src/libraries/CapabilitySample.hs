{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Libraries.CapabilitySample where

import Capability.Reader
import Capability.Sink
import Capability.Source
import Capability.State
import Capability.Writer
import Control.Monad.Reader (ReaderT (..))
import Data.IORef
import GHC.Generics

newtype App a = App {runApp :: ReaderT State IO a}
  deriving (Functor, Applicative, Monad)
  deriving
    (HasSource "counter" Int, HasSink "counter" Int, HasState "counter" Int)
    via ReaderIORef (Field "counter" () (MonadReader (ReaderT State IO)))
  deriving
    (HasSink "log" [String], HasWriter "log" [String])
    via WriterLog (ReaderIORef (Field "log" () (MonadReader (ReaderT State IO))))
  deriving
    (HasSource "config" String, HasReader "config" String)
    via (Field "config" () (MonadReader (ReaderT State IO)))

data State = State
  { counter :: IORef Int,
    log :: IORef [String],
    config :: String
  }
  deriving (Generic)

prog :: (HasReader "config" String m, HasState "counter" Int m, HasWriter "log" [String] m) => m (Int, [String])
prog = listen @"log" $ do
  tell @"log" ["first"]
  modify @"counter" (+ 1)
  modify @"counter" (+ 1)
  tell @"log" ["second"]
  ask @"config" >>= (tell @"log" . pure)
  get @"counter"

run :: IO (Int, [String])
run = do
  c <- newIORef 0
  l <- newIORef []
  flip runReaderT (State c l "conf") . runApp $ prog
