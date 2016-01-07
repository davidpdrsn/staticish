module Handlers
    ( buildHandlers
    )
  where

import Import
import Control.Monad.State
import Network.Wai
import Data.Aeson
import Data.Text.Lazy (Text)

import qualified Data.Map as M

buildHandlers :: State Handlers ()
buildHandlers = do
    addHandler "/all-posts.json" $ \posts _req -> toJSON posts

    addHandler "/search.json" $ \_posts _req -> toJSON ("Not implemented" :: String)

addHandler :: Text -> ([Post] -> Request -> Value) -> State Handlers ()
addHandler route f = get >>= put . M.insert route f
