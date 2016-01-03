module Types
    ( Posts
    )
  where

import Data.Map (Map)
import Data.Text.Lazy (Text)
import CompileMarkdown (CompiledMarkdown)

type Posts = Map Text CompiledMarkdown
