module HSL.HFP where

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Data.Aeson
       ((.:)
       ,FromJSON
       ,Value(Object)
       ,decode
       ,parseJSON
       ,withObject)
import Data.ByteString.Lazy
       (ByteString)
import Data.HashMap.Strict
       (elems)
import GHC.IO.Unsafe
       (unsafeInterleaveIO)
import Network.MQTT.Client
       (QoS(QoS0)
       ,_hostname
       ,_msgCB
       ,mqttConfig
       ,runClient
       ,subscribe
       ,waitForClient)

data Message =
  Message
  { operator :: Integer
  , vehicle :: Integer
  , stop :: Integer
  , latitude :: Double
  , longitude :: Double
  }
  deriving Show

instance FromJSON Message where
  parseJSON (Object o) = parseJSON' . head . elems $ o
    where
      parseJSON' (Object o) =
        Message
        <$> o .: "oper"
        <*> o .: "veh"
        <*> o .: "stop"
        <*> o .: "lat"
        <*> o .: "long"

messages :: IO (TQueue Message)
messages = do
  queue <- newTQueueIO
  mc <- runClient
    mqttConfig
      { _hostname = "mqtt.hsl.fi"
      , _msgCB = Just $ \_ _ m -> case decode m :: Maybe Message of
          Just message -> atomically $ writeTQueue queue message
          Nothing -> return ()
      }
  subscribe mc [("/hfp/v2/journey/ongoing/arr/#", QoS0)]
  return queue
