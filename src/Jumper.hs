module Jumper where

import HSL.Common
import HSL.HFP
import Naqsha.Geometry
import Naqsha.Geometry.Spherical

data State
  = Dangling
  | InVehicle {
    vehicle :: Vehicle,
    stop :: StopId,
    position :: Geo
  }
  | AtStop {
    stop :: StopId,
    vehicle :: Vehicle,
    position :: Geo
  }
  deriving Show


update :: State -> Event -> Maybe State
update -- jump to the first bus at the first stop
  Dangling
  Event {
    stop = Just s',
    vehicle = v',
    position = p'
  }
  = Just InVehicle {
    vehicle = v',
    stop = s',
    position = p'
  }
update -- jump at the first encountered stop
  InVehicle {
    vehicle = v',
    stop = s',
    position = p'
  }
  Event {
    vehicle = v'',
    stop = Just s'',
    position = p''
  }
  | v' == v'' && s' /= s'' = Just AtStop {
    stop = s'',
    vehicle = v'',
    position = p''
  }
  | otherwise = Nothing
update -- jump int first encountered bus inside 16m radius
  AtStop {
    vehicle = v',
    position = p'
  }
  Event {
    stop = Just s'',
    vehicle = v'',
    position = p''
  }
  | distance p' p'' < 16  && v' /= v'' = Just InVehicle {
    vehicle = v'',
    stop = s'',
    position = p''
  }
  | otherwise = Nothing
update -- update position of the vehicle
  InVehicle {
    vehicle = v',
    stop = s'
  }
  Event {
    vehicle = v'',
    position = p''
  }
  | v' == v'' = Just InVehicle {
    vehicle = v',
    stop = s',
    position = p''
  }
  | otherwise = Nothing
update _ _ = Nothing
