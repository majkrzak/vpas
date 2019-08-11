module HSL.Common where

data Vehicle = Vehicle Integer Integer
  deriving (Eq,Show)

data StopId = StopId Integer
  deriving (Eq,Show)

data Position = Position Double Double
  deriving (Eq,Show)

data Stop = Stop {
  stopId :: String,
  position :: Position
} deriving Show
