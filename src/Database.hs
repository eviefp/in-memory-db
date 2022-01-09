module Database
  ( -- | Key
    Key (getKey),
    mkKey,
    -- | Value
    Value (..),
    printValue,
    Index (..),
    -- | Database
    Database,
    create,
    insert,
    get,
    getIndex,
  )
where

import Control.Monad ((<=<))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M

newtype Key = Key
  { getKey :: String
  }
  deriving (Eq, Ord)

mkKey :: String -> Maybe Key
mkKey "" = Nothing
mkKey t = Just . Key $ t

data Value
  = VText !String
  | VInt !Integer
  | VTextList ![String]
  | VIntList ![Integer]
  deriving (Show)

printValue :: Value -> String
printValue =
  \case
    (VText s) -> show s
    (VInt n) -> show n
    (VTextList ss) -> show ss
    (VIntList ns) -> show ns

type Database = Map Key Value

create :: Database
create = M.empty

insert :: Key -> Value -> Database -> Database
insert = M.insert

get :: Key -> Database -> Maybe Value
get = M.lookup

newtype Index = Index Int

getIndex :: Key -> Index -> Database -> Maybe Value
getIndex k (Index i) = go <=< get k
  where
    go :: Value -> Maybe Value
    go =
      \case
        (VTextList txts) -> VText <$> at i txts
        (VIntList ns) -> VInt <$> at i ns
        _ -> Nothing

at :: Int -> [a] -> Maybe a
at 0 (x : _) = Just x
at n (_ : xs) = at (n - 1) xs
at _ _ = Nothing
