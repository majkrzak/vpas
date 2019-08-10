module HSL.Routing where

import Control.Monad.IO.Class
       (liftIO)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import HSL.Common
import Network.HTTP.Req
import Control.Arrow

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
          parseMaybe
            (withObject "data"
              (.: "data") >>> (>>=
                withObject "stops"
                  (.: "stops") >>> (>>=
                    withArray "stops"
                      (mapM
                        (withObject "stop" $ \o ->
                          Stop <$> o .: "gtfsId" <*> (Position <$> o .: "lat" <*> o .: "lon"))))))
            (responseBody r)
    case body' of
      Just body -> return . V.toList $ body
      Nothing -> return []
