module Types where

import Data.Map (Map)
import Data.Text.Lazy (Text)
import CompileMarkdown (CompiledMarkdown)

data Post = Post
          { postTitle :: Text
          , postBody  :: CompiledMarkdown
          }
          deriving (Show, Eq)

type Views = Map Text Text
