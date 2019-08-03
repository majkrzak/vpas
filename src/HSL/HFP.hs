module HSL.HFP where

import Control.Concurrent
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
  }
  deriving Show

instance FromJSON Message where
  parseJSON (Object o) = parseJSON' . head . elems $ o
    where
      parseJSON' (Object o) =
        Message <$> o .: "oper" <*> o .: "veh" <*> o .: "stop"

messages :: IO [Message]
messages = do
  msg <- newEmptyMVar
  mc <- runClient
    mqttConfig
      { _hostname = "mqtt.hsl.fi"
      , _msgCB = Just (\_ t m -> putMVar msg m)
      }
  subscribe mc [("/hfp/v2/journey/ongoing/arr/#", QoS0)]
  messages' msg
  where
    messages' :: MVar ByteString -> IO [Message]
    messages' msg = unsafeInterleaveIO $ do
      payload <- takeMVar msg
      case decode payload :: Maybe Message of
        Just message -> (message :) <$> messages' msg
        Nothing -> messages' msg
