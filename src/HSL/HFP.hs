module HSL.HFP where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), Value(Object), decode)
import Data.HashMap.Strict (elems)
import Control.Concurrent
import Network.MQTT.Client (mqttConfig, _msgCB, _hostname, runClient, subscribe, waitForClient, QoS(QoS0))
import Data.ByteString.Lazy (ByteString)
import GHC.IO.Unsafe (unsafeInterleaveIO)

data Message = Message {
  operator :: Integer,
  vehicle :: Integer,
  stop :: Integer
} deriving Show


instance FromJSON Message where
  parseJSON (Object o ) = parseJSON' (head (elems o))
   where parseJSON' (Object o) = Message <$> o .: "oper" <*> o .: "veh" <*> o .: "stop"


messages :: IO [Message]
messages = do
 msg <- newEmptyMVar
 mc <- runClient mqttConfig{
   _hostname = "mqtt.hsl.fi",
   _msgCB = Just (\_ t m -> putMVar msg m)
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
