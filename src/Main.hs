module Main
    ( main
    )
  where

import App
import CompileMarkdown
import Control.Monad
import Control.Monad.State
import Data.Aeson
import Data.List
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Import
import Mutex
import Network.Wai
import Network.Wai.Handler.Warp
import System.Directory
import System.FilePath.Posix

import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = do
    posts <- compileAllPostsInDir "posts"
    views <- findViewsInDir "views"
    layout <- id T.readFile "views/layout.html"
    mutex <- newMutex
    putStrLn $ "Listening on post " ++ show port
    let handlerMap = execState buildHandlers M.empty
    run port (app mutex layout posts views handlerMap)

buildHandlers :: State Handlers ()
buildHandlers = do
    addHandler "/all-posts" $ \posts _req -> toJSON posts

addHandler :: Text -> ([Post] -> Request -> Value) -> State Handlers ()
addHandler route f = do
    st <- get
    put $ M.insert route f st

port :: Port
port = 4000

compileAllPostsInDir :: FilePath -> IO (Map Text Post)
compileAllPostsInDir dir = do
    filesInDir <- getFilesInDir dir
    contents <- mapM T.readFile filesInDir
    return $ compileMarkdownPosts $ zip filesInDir contents

findViewsInDir :: FilePath -> IO Views
findViewsInDir dir = do
    filesInDir <- filter (not . isLayout) <$> getFilesInDir dir
    contents <- mapM T.readFile filesInDir
    return $ M.fromList $ zip (map (getViewName . cs) filesInDir) contents

getViewName :: Text -> Text
getViewName = ("/" `mappend`) . cs . takeBaseName . cs

isLayout :: FilePath -> Bool
isLayout = isInfixOf "layout"

getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir dir = do
    allFiles <- map ((dir ++ "/") ++) <$> getDirectoryContents dir
    filterM doesFileExist allFiles

compileMarkdownPosts :: [(FilePath, Text)] -> Map Text Post
compileMarkdownPosts filesAndContents =
    let markdownFiles = filter (isMarkdownFile . fst) filesAndContents
        (filenames, contents) = unzip markdownFiles
        postTitles = map (cs . takeBaseName) filenames
        compiledContents = map compileMarkdown contents
        posts = zipWith Post postTitles compiledContents
    in M.fromList $ zip postTitles posts

isMarkdownFile :: FilePath -> Bool
isMarkdownFile = (`elem` markdownExtensions) . takeExtension
    where markdownExtensions = [ ".markdown"
                               , ".md"
                               ]
