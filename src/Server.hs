{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Server
  ( run,
  )
where

import Control.Monad (forever, (>>=))
import Data.Functor (($>))
import Data.IORef (IORef)
import Data.IORef qualified as Ref
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Database (Database)
import Database qualified as Db
import Parser (Command (Get, Insert), parseCommand)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)
import Text.Megaparsec qualified as P
import Prelude (IO, Maybe (Just, Nothing), Show (show), pure, (.), (<$>))

run :: IO ()
run = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  db <- Ref.newIORef Db.create
  forever (interact (loop db))

loop :: IORef Database -> Text -> IO Text
loop db line = do
  case P.parseMaybe parseCommand line of
    Nothing -> pure "Parse error"
    Just cmd -> case cmd of
      Insert k v -> Ref.modifyIORef db (Db.insert k v) $> "done"
      Get k -> T.pack . show . Db.get k <$> Ref.readIORef db

interact :: (Text -> IO Text) -> IO ()
interact l = do
  T.putStr "δ. "
  line <- T.getLine
  l line >>= T.putStrLn
