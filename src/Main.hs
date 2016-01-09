module Main
    ( main
    , runApp
    )
  where

import App
import CompileMarkdown
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Text.Lazy (Text)
import Handlers
import Import
import Mutex
import Network.Wai.Handler.Warp
import System.Directory
import System.Environment
import System.FilePath.Posix

import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = do
    args <- getArgs
    fromMaybe (unknownArgs args) (parseArgs args)

unknownArgs :: [String] -> IO ()
unknownArgs [] = do putStrLn "No arguments"
                    putStrLn ""
                    showDoc
unknownArgs args = do putStr "Unknown arguments "
                      print args
                      putStrLn ""
                      showDoc

showDoc :: IO ()
showDoc = putStrLn doc
    where doc = intercalate "\n" [ "Staticish help"
                                 , ""
                                 , "  server, s      # Start the server"
                                 , "  --help, -h     # Show this message"
                                 , ""
                                 ]

parseArgs :: [String] -> Maybe (IO ())
parseArgs [arg] = M.lookup arg supportedArgs
parseArgs _ = Nothing

supportedArgs :: Map String (IO ())
supportedArgs = M.fromList [ ("server", runApp)
                           , ("s", runApp)
                           , ("--help", showDoc)
                           , ("-h", showDoc)
                           ]

runApp :: IO ()
runApp = do
    posts <- compileAllPostsInDir "posts"
    views <- findViewsInDir "views"
    layout <- T.readFile "views/layout.html"
    mutex <- newMutex
    putStrLn $ "Listening on post " ++ show port
    let handlerMap = execState buildHandlers M.empty
    run port (app mutex layout posts views handlerMap)

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
