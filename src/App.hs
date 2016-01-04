module App
    ( app
    )
  where

import Import
import CompileMarkdown
import Network.Wai
import Control.Exception
import Network.HTTP.Types
import Data.CaseInsensitive (CI)
import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)
import Mutex
import Control.Concurrent
import Control.Monad

import qualified Data.Map as M

app :: Mutex -> Posts -> Application
app mutex posts req respond = bracket_ before after (respond response)
    where
      response = case postForRequest posts req of
                  Nothing -> respond404
                  Just post -> respondWithPost post
      before = logRequest mutex req
      after = logResponse mutex req response

-- | Finding the matching post

postForRequest :: Posts -> Request -> Maybe CompiledMarkdown
postForRequest posts req = do
    let path = cs <$> pathInfo req
    name <- postNameFromPath path
    M.lookup name posts

postNameFromPath :: [Text] -> Maybe Text
postNameFromPath ["posts", name] = Just name
postNameFromPath _ = Nothing

responseT :: Status -> ResponseHeaders -> Text -> Response
responseT status headers text = responseLBS status headers $ cs text

-- | Response convenience functions

respond404 :: Response
respond404 = responseT notFound404 [] "Not found"

respondWithPost :: CompiledMarkdown -> Response
respondWithPost post = responseT ok200 headers html
     where
       html = getHtmlText post

       headers :: [(CI ByteString, ByteString)]
       headers = [("Content-Type", "text/html")]

-- | Logging

logRequest :: Mutex -> Request -> IO ()
logRequest mutex req = void $ forkIO $ withMutex mutex $ do
    putStr "Starting "
    logRequestPath req

logResponse :: Mutex -> Request -> Response -> IO ()
logResponse mutex req _res = void $ forkIO $ withMutex mutex $ do
    putStr "Finishing "
    logRequestPath req

logRequestPath :: Request -> IO ()
logRequestPath req = do
    putStr $ cs $ requestMethod req
    putStr " "
    putStrLn $ cs $ rawPathInfo req
