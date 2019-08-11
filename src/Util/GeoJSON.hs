module Util.GeoJSON where

import Data.Aeson
import Naqsha.Geometry
import GHC.Exts

linestring :: [Geo] -> Value
linestring coordinates = Object $ fromList [
  ("type", String "LineString"),
  ("coordinates", Array $ fromList (
    fmap (\(Geo lat lon) -> Array $ fromList [
      Number . toDegree . toAngle $ lon,
      Number . toDegree . toAngle $ lat
    ] ) coordinates
  ))]