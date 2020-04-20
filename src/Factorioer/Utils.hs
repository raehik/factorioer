module Factorioer.Utils where

import Data.Text (Text)
import Data.Text as T

tshow :: Show a => a -> Text
tshow = T.pack . show
