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
import Text.Regex
import Control.Applicative
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as T

app :: Mutex -> Posts -> Application
app mutex posts req respond = do
    layout <- T.readFile "views/layout.html"
    let
      response = fromJust $ respondWithPost layout <$> postForRequest posts req
                        <|> Just respond404
      before = logRequest mutex req
      after = logResponse mutex req response
    bracket_ before after (respond response)

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

respondWithPost :: Text -> CompiledMarkdown -> Response
respondWithPost layout post = responseT ok200 headers html
     where
       html = applyLayout layout $ getHtmlText post

       headers :: [(CI ByteString, ByteString)]
       headers = [("Content-Type", "text/html")]

applyLayout :: Text -> Text -> Text
applyLayout layout html = let regex = mkRegexT "{{ *yield *}}"
                          in subRegexT regex layout html

mkRegexT :: Text -> Regex
mkRegexT = mkRegex . cs

subRegexT :: Regex -> Text -> Text -> Text
subRegexT a b c = cs $ subRegex a (cs b) (cs c)

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
