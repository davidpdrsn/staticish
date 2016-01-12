module Network.Staticish
    ( defaultMain
    )
  where

import Control.Monad
import Control.Monad.State
import Data.List
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Network.Staticish.App
import Network.Staticish.CompileMarkdown
import Network.Staticish.Handlers
import Network.Staticish.Import
import Network.Staticish.Mutex
import Network.Staticish.OptionParsing
import Network.Wai.Handler.Warp
import Options.Applicative
import System.Directory
import System.FilePath.Posix

import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as T

defaultMain :: IO ()
defaultMain = runApp =<< execParser (parseCommand `withInfo` "Staticish site generator")

runApp :: Command -> IO ()
runApp (Server port) = do
    putStrLn $ "Listening on port " ++ show port
    run port =<< (app <$> newMutex
                      <*> T.readFile "views/layout.html"
                      <*> compileAllPostsInDir "posts"
                      <*> findViewsInDir "views"
                      <*> pure (execState buildHandlers M.empty))
runApp (New path) = do
    putStrLn "Not supported yet"
    putStrLn path

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
