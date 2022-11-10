{-# LANGUAGE LambdaCase #-}

module Inquiry.Http (http) where

import Data.Function ((&))
import Data.Text.Lazy.Encoding (decodeUtf8)
import Inquiry.Request (Method (..), Request, Response (Response), reqMethod)
import Lens.Micro.Platform ((^.))
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (getGlobalManager)
import qualified Network.HTTP.Types as Http
import qualified Data.Text.Lazy as T

http :: Request -> IO Response
http request = do
  manager <- getGlobalManager
  let httpRequest =
        Http.defaultRequest
          { Http.method = request ^. reqMethod & toMethod
          }
  response <- Http.httpLbs httpRequest manager
  let statusCode = Http.statusCode $ Http.responseStatus response
      body = T.toStrict $ decodeUtf8 (Http.responseBody response)
  pure $ Response statusCode body

toMethod :: Method -> Http.Method
toMethod = \case
  GET -> Http.methodGet
  POST -> Http.methodPost
