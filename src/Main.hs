module Main where

import CompileMarkdown
import Network.Wai
import Control.Exception
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Data.CaseInsensitive (CI)
import Data.ByteString (ByteString)
import Data.String.Conversions
import Data.Map (Map)
import Data.Text.Lazy (Text)
import System.Directory
import System.FilePath.Posix

import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = do
    posts <- compileAllPosts
    run 4000 (app posts)

type Posts = Map Text CompiledMarkdown

compileAllPosts :: IO Posts
compileAllPosts = do
    paths <- filter isMarkdownPost <$> map ("posts/" ++) <$> getDirectoryContents "posts"
    contents <- map compileMarkdown <$> mapM T.readFile paths
    let
      paths' = map (cs . takeBaseName) paths :: [Text]
      posts = M.fromList $ zip paths' contents
    return posts

isMarkdownPost :: FilePath -> Bool
isMarkdownPost = (== ".markdown") . takeExtension

app :: Posts -> Application
app posts req respond = bracket_ before after handler
    where
      handler = case postForRequest posts req of
                  Nothing -> respond404 respond
                  Just post -> respondWithPost respond post

      before = return ()
      after = return ()

respond404 :: (Response -> a) -> a
respond404 respond = respond $ responseT notFound404 [] "Not found"

respondWithPost :: (Response -> a) -> CompiledMarkdown -> a
respondWithPost respond post = respond $ responseT ok200 headers html
     where
       html = getHtmlText post

       headers :: [(CI ByteString, ByteString)]
       headers = [("Content-Type", "text/html")]

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
