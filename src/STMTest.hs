module STMTest where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Cont

inc :: TVar Int -> STM ()
inc = flip modifyTVar (+ 1)

reset :: TVar Int -> STM ()
reset v = do
  current <- readTVar v
  check (current == 2)
  writeTVar v 0

test :: IO Int
test = flip runContT return $ do
  tvar <- lift . atomically $ newTVar 0
  async3 <- ContT $ withAsync (atomically $ reset tvar)
  async <- ContT $ withAsync (atomically $ inc tvar)
  async2 <- ContT $ withAsync (atomically $ inc tvar)
  lift $ do
    wait async
    wait async2
    wait async3
    atomically $ readTVar tvar
