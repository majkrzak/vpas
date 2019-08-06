module Util.Stream (Stream,runStream,foldStream,mapStream) where

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

foldStream :: (a -> b -> IO b) -> b -> Stream a -> IO b
foldStream f s (Stream read) = do
  n <- atomically read
  m <- f n s
  foldStream f m (Stream read)

mapStream :: (a -> b) -> Stream a -> Stream b
mapStream f (Stream read) = Stream $ f <$> read

zipStream :: [Stream a] -> Stream a
zipStream = undefined
