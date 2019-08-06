module Main where

import HSL.HFP
import System.IO
import Util.Stream
import Jumper

loop event state = do
  hFlush stdout
  case update state event of
    Just next' -> print next' >> return next'
    Nothing -> return state


main = foldStreamIO loop Dangling =<< events
