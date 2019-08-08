module Util.Stream (Stream,runStream) where

import Control.Concurrent.STM
import System.IO.Unsafe

newtype Stream a = Stream (STM a)

runStream
  :: ((a -> IO ()) -> IO i)
  -> IO (Stream a)
runStream block = do
  queue <- newTQueueIO
  block (atomically . writeTQueue queue)
  return $ Stream (readTQueue queue)

instance Foldable Stream where
  foldMap f (Stream read) = f (unsafePerformIO $ atomically read) <> foldMap f (Stream read)

instance Functor Stream where
  fmap f (Stream read) = Stream $ f <$> read

instance Semigroup (Stream a) where
  (<>) (Stream lhs) (Stream rhs) = Stream $ lhs `orElse` rhs
