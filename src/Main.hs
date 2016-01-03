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
    markdownFilePaths <- filter isMarkdownFile <$> map (dir' ++) <$> getDirectoryContents dir
    contents <- map compileMarkdown <$> mapM T.readFile markdownFilePaths
    let
      markdownFileNames = map (cs . takeBaseName) markdownFilePaths
      posts = M.fromList $ zip markdownFileNames contents
    return posts

isMarkdownFile :: FilePath -> Bool
isMarkdownFile = (`elem` markdownExtensions) . takeExtension
    where markdownExtensions = [ ".markdown"
                               , ".md"
                               , ".mkd"
                               ]
