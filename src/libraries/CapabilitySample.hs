{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Libraries.CapabilitySample where

import Capability.Reader
import Capability.Sink
import Capability.Source
import Capability.State
import Control.Monad.Reader (ReaderT (..))
import Data.IORef
import GHC.Generics

newtype App a = App {runApp :: ReaderT State IO a}
  deriving (Functor, Applicative, Monad)
  deriving
    (HasSource "counter" Int, HasSink "counter" Int, HasState "counter" Int)
    via ReaderIORef (Field "counter" () (MonadReader (ReaderT State IO)))

data State = State
  { counter :: IORef Int
  }
  deriving (Generic)

prog :: HasState "counter" Int m => m Int
prog = do
  modify @"counter" (+1)
  modify @"counter" (+1)
  get @"counter"

run :: IO Int
run = do
  ref <- newIORef 0
  flip runReaderT (State ref) . runApp $ prog
