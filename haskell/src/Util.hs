module Util (tshow) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Show a value as Text.
tshow :: (Show a) => a -> Text
tshow = T.pack . show