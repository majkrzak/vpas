module HSL.Routing(stops) where

import Control.Arrow
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Vector
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
    let body' =
          parse
            (withObject "data"
              (.: "data") >>> (>>=
                withObject "stops"
                  (.: "stops") >>> (>>=
                    withArray "stops"
                      (toList >>> traverse
                        (withObject "stop" $ \o ->
                          Stop
                            <$> o .: "gtfsId"
                            <*> (Position
                              <$> o .: "lat"
                              <*> o .: "lon"))))))
            (responseBody r)
    case body' of
      Success body -> return body
      Error error -> fail error
