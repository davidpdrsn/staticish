module Main
    ( main
    )
  where

import Import
import CompileMarkdown
import Network.Wai.Handler.Warp
import Data.Text.Lazy (Text)
import System.Directory
import System.FilePath.Posix
import App

import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = do
    putStrLn "Compiling posts"
    posts <- compileAllPostsInDir "posts"
    putStrLn "Compiled posts"
    putStrLn $ "Listening on post " ++ show port
    run port (app posts)

port :: Port
port = 4000

compileAllPostsInDir :: FilePath -> IO Posts
compileAllPostsInDir dir = do
    let dir' = dir ++ "/"
    paths <- filter isMarkdownPost <$> map (dir' ++) <$> getDirectoryContents dir
    contents <- map compileMarkdown <$> mapM T.readFile paths
    let
      paths' = map (cs . takeBaseName) paths :: [Text]
      posts = M.fromList $ zip paths' contents
    return posts

isMarkdownPost :: FilePath -> Bool
isMarkdownPost = (== ".markdown") . takeExtension
