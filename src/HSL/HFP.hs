module HSL.HFP where

import Data.Aeson
import Data.HashMap.Strict
import HSL.Common
import Network.MQTT.Client
import Util.Stream

data Event =
  Event
  { position :: Position
  , vehicle :: Vehicle
  , stop :: Maybe StopId
  }
  deriving Show

instance FromJSON Event where
  parseJSON =
    withObject "Event'"
    $ withObject
      "Event"
      (\o -> Event
       <$> (Position
            <$> o .: "lat"
            <*> o .: "long")
       <*> (Vehicle
            <$> o .: "oper"
            <*> o .: "veh")
       <*> (do
              val <- o .:? "stop"
              return
                $ StopId
                <$> val))
    . head
    . elems

events :: IO (Stream Event)
events = runStream $ \write -> do
  mc <- runClient
    mqttConfig
    { _hostname = "mqtt.hsl.fi"
    , _msgCB = Just $ \_ _ m -> case decode m :: Maybe Event of
        Just message -> write message
        Nothing -> return ()
    }
  subscribe mc [("/hfp/v2/journey/ongoing/vp/#", QoS0)]
