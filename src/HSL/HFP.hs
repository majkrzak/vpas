module HSL.HFP where

import Data.Aeson
import Data.Aeson.Parser
import Data.HashMap.Strict
import HSL.Common
import Network.MQTT.Client
import Util.Stream
import Naqsha.Geometry


data Event =
  Event
  { position :: Geo
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
       <$> (Geo
            <$> (lat . degree . (toRational::Double->Rational) <$> o .: "lat" )
            <*> (lon . degree . (toRational::Double->Rational) <$> o .: "long"))
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
    , _msgCB = Just $ \_ _ m -> case eitherDecode m :: Either String Event of
        Right message -> write message
        Left error -> print error >> fail error
    }
  subscribe mc [("/hfp/v2/journey/ongoing/vp/#", QoS0)]
