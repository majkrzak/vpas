module Util.Stream (Stream,runStream,fold) where

import Control.Concurrent.STM

newtype Stream a =
  Stream (TQueue a)

runStream
  :: ((a -> IO ()) -> IO i)
  -> IO (Stream a)
runStream block = do
  queue <- newTQueueIO
  block (atomically . writeTQueue queue)
  return $ Stream queue

fold :: (a -> b -> IO b) -> b -> Stream a -> IO b
fold f s (Stream q) = do
  n <- atomically $ readTQueue q
  r <- f n s
  fold f r (Stream q)
