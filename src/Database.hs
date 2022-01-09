module Database
  ( -- | Key
    Key (getKey),
    mkKey,
    -- | Value
    Value (..),
    -- | Database
    Database,
    create,
    insert,
    get,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Prelude (Eq, Integer, Maybe (Just, Nothing), Ord, Show, ($), (.))

newtype Key = Key
  { getKey :: Text
  }
  deriving (Eq, Ord)

mkKey :: Text -> Maybe Key
mkKey "" = Nothing
mkKey t = Just . Key $ t

data Value
  = VText !Text
  | VInt !Integer
  | VTextList ![Text]
  | VIntList ![Integer]
  deriving (Show)

type Database = Map Key Value

create :: Database
create = M.empty

insert :: Key -> Value -> Database -> Database
insert = M.insert

get :: Key -> Database -> Maybe Value
get = M.lookup
