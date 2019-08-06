module Jumper where

import HSL.Common
import HSL.HFP

data State
  = Dangling
  | InVehicle {
    vehicle :: Vehicle,
    stop :: Stop,
    position :: Position
  }
  | AtStop {
    stop :: Stop,
    vehicle :: Vehicle,
    position :: Position
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
update -- jump int first encountered bus
  AtStop {
    stop = s',
    vehicle = v'
  }
  Event {
    stop = Just s'',
    vehicle = v'',
    position = p''
  }
  | s' == s'' && v' /= v'' = Just InVehicle {
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
