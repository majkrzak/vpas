module Main where

import HSL.HFP
import Jumper
import System.IO
import Util.Stream
import Control.Monad
import Data.Foldable (toList)

dedup (x:xs) = x : dedup' x xs
  where
    dedup' x1 (x2:xs)
      | x1 == x2 = dedup' x1 xs
      | otherwise = dedup (x2:xs)

main = do
  events' <- events
  let states = scanl update' Dangling (toList events')
  let states' = dedup states
  foldM (\_ s -> hFlush stdout >> print s) () states'
