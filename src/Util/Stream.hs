module Util.Stream where

import Control.Concurrent.STM

newtype Stream a =
  Stream (TQueue a)

fold :: (a -> b -> IO b) -> b -> Stream a -> IO b
fold f s (Stream q) = do
  n <- atomically $ readTQueue q
  r <- f n s
  fold f r (Stream q)
