module OptionParsing
    ( Command(..)
    , parseCommand
    , withInfo
    )
  where

import Options.Applicative
import Network.Wai.Handler.Warp (Port)

data Command = Server Port
             | New FilePath
             deriving (Show)

parseCommand :: Parser Command
parseCommand = subparser $
    command "server" (parseServer `withInfo` "Start the server") <>
    command "new" (parseNew `withInfo` "Scaffold a new empty site")

parseServer :: Parser Command
parseServer = Server <$> parsePort

parsePort :: Parser Port
parsePort = option auto $
    short 'p' <>
    long "port" <>
    metavar "PORT" <>
    help "Port to run on (default is 4000)" <>
    value 4000

parseNew :: Parser Command
parseNew = New <$> argument str (metavar "PATH")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
