module Main
    ( main
    )
  where

import Import
import CompileMarkdown
import Network.Wai.Handler.Warp
import System.Directory
import System.FilePath.Posix
import App
import Data.Text.Lazy (Text)
import Control.Monad
import Mutex
import Data.List

import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = do
    posts <- compileAllPostsInDir "posts"
    views <- findViewsInDir "views"
    mutex <- newMutex
    putStrLn $ "Listening on post " ++ show port
    run port (app mutex posts views)

port :: Port
port = 4000

compileAllPostsInDir :: FilePath -> IO Posts
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

compileMarkdownPosts :: [(FilePath, Text)] -> Posts
compileMarkdownPosts filesAndContents =
    let markdownFiles = filter (isMarkdownFile . fst) filesAndContents
        (filenames, contents) = unzip markdownFiles
        postTitles = map (cs . takeBaseName) filenames
        compiledContents = map compileMarkdown contents
    in M.fromList $ zip postTitles compiledContents

isMarkdownFile :: FilePath -> Bool
isMarkdownFile = (`elem` markdownExtensions) . takeExtension
    where markdownExtensions = [ ".markdown"
                               , ".md"
                               ]
