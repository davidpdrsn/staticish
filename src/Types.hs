module Types where

import CompileMarkdown (CompiledMarkdown)
import Data.Aeson
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Network.Wai

data Post = Post
          { postTitle :: Text
          , postBody  :: CompiledMarkdown
          }
          deriving (Show, Eq)

instance ToJSON Post where
  toJSON (Post title body) = object ["title" .= title, "body" .= body]

type Views = Map Text Text

type Handlers = Map Text ([Post] -> Request -> Value)
