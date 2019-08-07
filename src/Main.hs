module Main where

import HSL.HFP
import Jumper
import System.IO
import Util.Stream
import Control.Monad

loop state event = do
  hFlush stdout
  case update state event of
    Just next' -> print next' >> return next'
    Nothing -> return state

main = foldM loop Dangling =<< events
