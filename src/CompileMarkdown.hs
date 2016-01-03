module CompileMarkdown
    ( compileMarkdown

    , CompiledMarkdown
    , getHtmlText
    )
  where

import Text.Markdown
import Data.Text.Lazy (Text)
import Text.Blaze.Html.Renderer.Text

compileMarkdown :: Text -> CompiledMarkdown
compileMarkdown = CompiledMarkdown . renderHtml . markdown defaultSettings
    where defaultSettings = def

newtype CompiledMarkdown = CompiledMarkdown { getHtmlText :: Text }
