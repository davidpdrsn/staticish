module Network.Staticish.CompileMarkdown
    ( compileMarkdown

    , CompiledMarkdown
    , getHtmlText
    )
  where

import Data.Aeson.Types
import Data.Text.Lazy (Text)
import Text.Blaze.Html.Renderer.Text
import Text.Markdown
import Data.String.Conversions

compileMarkdown :: Text -> CompiledMarkdown
compileMarkdown = CompiledMarkdown . renderHtml . markdown defaultSettings
    where defaultSettings = def

newtype CompiledMarkdown = CompiledMarkdown { getHtmlText :: Text }
                           deriving (Show, Eq)

instance ToJSON CompiledMarkdown where
  toJSON (CompiledMarkdown html) = String $ cs html
