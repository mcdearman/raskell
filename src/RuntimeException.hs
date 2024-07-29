module RuntimeException where

import Data.Text

newtype RuntimeException = RuntimeException Text

instance Show RuntimeException where
  show (RuntimeException msg) = "Runtime error: " ++ unpack msg