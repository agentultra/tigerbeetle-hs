module Database.TigerBeetle.Address where

import Data.Text (Text)

-- | A host address of the Tigerbeetle instance
newtype Address = Address {getAddress :: Text}
  deriving (Eq, Show)
