import System.IO
import Control.Exception
import Control.Monad.Trans


foldFile :: String -> IO Int
foldFile path = bracket (openFile path ReadMode) (hClose) $ \handle ->
  loop handle 0 0
  where
    loop :: Handle -> Int -> Int -> IO Int
    loop h count lineCount = do
      eof <- hIsEOF h
      if eof || (lineCount == 3) then return count else do
        next <- hGetLine h
        if length next < 100 then  loop h count lineCount else loop h (count + (length . filter (== 'x') $  next)) (lineCount + 1)

data Stream a = EOF | Chunks [a]

data Iteratee i m o = Done o | Cont (Maybe String) (Stream i -> m (Iteratee i m o, Stream i))

instance Functor (Iteratee i m) where
  fmap = undefined

instance (Monad m) => Applicative (Iteratee i m) where
  pure = return

  (<*>) = undefined

instance Monad m => Monad (Iteratee i m) where
  return = Done

  it >>= f = case it of
    Done v -> f v
    Cont e k -> Cont e (\str -> k str >>= \(it', str') -> return (it' >>= f, str'))

instance MonadTrans (Iteratee i) where
  lift ma = Cont Nothing (\str -> ma >>= \a -> return (Done a, Chunks []))

instance MonadIO m => MonadIO (Iteratee i m) where
  liftIO = lift . liftIO
