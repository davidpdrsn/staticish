module Types where

import Data.Map (Map)
import Data.Text.Lazy (Text)
import CompileMarkdown (CompiledMarkdown)

type Posts = Map Text CompiledMarkdown
type Views = Map Text Text
