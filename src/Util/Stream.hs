module Util.Stream (Stream,runStream,mapStream) where

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

mapStream :: (a -> b) -> Stream a -> Stream b
mapStream f (Stream read) = Stream $ f <$> read

zipStream :: [Stream a] -> Stream a
zipStream = undefined
