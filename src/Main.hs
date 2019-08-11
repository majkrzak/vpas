module Main where

import HSL.HFP
import HSL.Common
import Jumper
import System.IO
import Util.Stream
import Util.GeoJSON
import Data.Aeson
import Control.Monad
import Data.Foldable (toList)
import Naqsha.Geometry
import qualified Data.ByteString.Lazy.Char8 as B

dedup (x:xs) = x : dedup' x xs
  where
    dedup' x1 (x2:xs)
      | x1 == x2 = dedup' x1 xs
      | otherwise = dedup (x2:xs)

main = do
  events' <- events
  let states = scanl update' Dangling (toList events')
  let states' = dedup states
  let filtered = tail $ take 256 states'
  let coordinates = fmap (\case
             InVehicle {..} -> position
             AtStop {..} -> position) filtered
  B.putStrLn . encode . linestring $ coordinates
