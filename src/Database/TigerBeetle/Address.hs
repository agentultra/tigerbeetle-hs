module Database.TigerBeetle.Address where

import Data.Text (Text)

newtype Address = Address {getAddress :: Text}
  deriving (Eq, Show)
