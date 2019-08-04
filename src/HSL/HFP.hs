module HSL.HFP where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Data.Aeson
import Data.HashMap.Strict
import HSL.Common
import Network.MQTT.Client

data Event =
  Event
  { position :: Position
  , vehicle :: Vehicle
  , stop :: Maybe Stop
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
                $ Stop
                <$> val))
    . head
    . elems

events :: IO (TQueue Event)
events = do
  queue <- newTQueueIO
  mc <- runClient
    mqttConfig
    { _hostname = "mqtt.hsl.fi"
    , _msgCB = Just $ \_ _ m -> case decode m :: Maybe Event of
        Just message -> atomically $ writeTQueue queue message
        Nothing -> return ()
    }
  subscribe mc [("/hfp/v2/journey/ongoing/vp/#", QoS0)]
  return queue
