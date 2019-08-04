module Main where

import HSL.HFP
import System.IO
import Util.Stream

dummy event _ = do
  print event
  hFlush stdout
  return ()

main = fold dummy () =<< events
