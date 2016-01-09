module App
    ( app
    )
  where

import CompileMarkdown
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder
import Import
import Mutex
import Network.HTTP.Types
import Network.Wai
import System.Directory
import Text.Regex

import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as T

app :: Mutex -> Text -> Map Text Post -> Views -> Handlers -> Application
app mutex layout posts views handlers req respond = do
    staticResponse <- staticFileForRequest req
    let
      response = fromMaybe respond404 $
                   respondWithFile <$> staticResponse
               <|> respondWithHtml layout <$> getHtmlText <$> postForRequest posts req
               <|> respondWithHtml layout <$> viewForRoot views req
               <|> respondWithHtml layout <$> viewForRequest views req
               <|> respondWithJson <$> jsonForRequest (M.elems posts) handlers req
      before = logRequest mutex req
      after = logResponse mutex req response
    bracket_ before after (respond response)

-- | Finding the matching post/view

postForRequest :: (Map Text Post) -> Request -> Maybe CompiledMarkdown
postForRequest posts req = do
    let path = cs <$> pathInfo req
    name <- postNameFromPath path
    M.lookup name (M.map postBody posts)

postNameFromPath :: [Text] -> Maybe Text
postNameFromPath ["posts", name] = Just name
postNameFromPath _ = Nothing

viewForRequest :: Views -> Request -> Maybe Text
viewForRequest views req = M.lookup (cs $ rawPathInfo req) views

viewForRoot :: Views -> Request -> Maybe Text
viewForRoot views req = if rawPathInfo req == "/"
                          then M.lookup "/index" views
                          else Nothing

staticFileForRequest :: Request -> IO (Maybe Text)
staticFileForRequest req = do
    let path = cs $ "public/" <> rawPathInfo req
    exists <- doesFileExist path
    if exists
      then Just <$> T.readFile path
      else return Nothing

jsonForRequest :: [Post] -> Handlers -> Request -> Maybe Value
jsonForRequest posts handlers req = do
    f <- M.lookup (cs $ rawPathInfo req) handlers
    return $ f posts req

-- | Response convenience functions

respond404 :: Response
respond404 = responseT notFound404 [] "Not found"

responseT :: Status -> ResponseHeaders -> Text -> Response
responseT status headers text = responseLBS status headers $ cs text

respondWithHtml :: Text -> Text -> Response
respondWithHtml layout post = responseT ok200 headers (applyLayout layout post)
     where
       headers :: [(CI ByteString, ByteString)]
       headers = [("Content-Type", "text/html")]

respondWithFile :: Text -> Response
respondWithFile = responseT ok200 []

respondWithJson :: Value -> Response
respondWithJson json = responseT ok200 headers $ toLazyText $ encodeToTextBuilder json
     where
       headers :: [(CI ByteString, ByteString)]
       headers = [("Content-Type", "application/json")]

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
    putStr $ cs $ requestMethod req
    putStr " "
    putStrLn $ cs $ rawPathInfo req

logResponse :: Mutex -> Request -> Response -> IO ()
logResponse mutex req res = void $ forkIO $ withMutex mutex $ do
    putStr "Finishing "
    putStr $ cs $ requestMethod req
    putStr " "
    putStr $ show $ statusCode $ responseStatus res
    putStr " "
    putStrLn $ cs $ rawPathInfo req
