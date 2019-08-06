module Main where

import HSL.HFP
import Jumper
import System.IO
import Util.Stream

loop event state = do
  hFlush stdout
  case update state event of
    Just next' -> print next' >> return next'
    Nothing -> return state

main = foldStream loop Dangling =<< events
