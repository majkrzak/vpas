module HSL.Routing where

import Control.Monad.IO.Class
       (liftIO)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import HSL.Common
import Network.HTTP.Req

stops :: IO [Stop]
stops =
  runReq defaultHttpConfig $ do
    r <-
      req
        POST
        (https "api.digitransit.fi" /: "routing/v1/routers/hsl/index/graphql")
        (ReqBodyBs "{stops{gtfsId lat lon}}")
        jsonResponse
        mempty
    let body = responseBody r
    let body' =
          parseMaybe
            (withObject
               "data"
               (\o ->
                  o .: "data" >>=
                  withObject
                    "stops"
                    (\o ->
                       o .: "stops" >>=
                       withArray
                         "stops"
                         (mapM
                            (withObject "stop" $ \o ->
                               Stop <$> o .: "gtfsId" <*> (Position <$> o .: "lat" <*> o .: "lon"))))))
            body
    case body' of
      Just body -> return . V.toList $ body
      Nothing -> return []
