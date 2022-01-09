module Server
  ( run,
  )
where

import Control.Monad (forever)
import Data.IORef (IORef)
import Data.IORef qualified as Ref
import Data.Text.IO qualified as T
import Database (Database)
import Database qualified as Db
import Eval (eval)
import Parser (parseCommand)
import System.Exit (exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Megaparsec qualified as P

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  Ref.newIORef Db.create >>= forever . repl . loop

loop :: IORef Database -> String -> IO ()
loop ref line = do
  case P.parseMaybe parseCommand line of
    Nothing -> putStrLn "Parse error."
    Just cmd -> do
      db <- Ref.readIORef ref
      eval onInsert onGet onGet onExit db cmd
  where
    onInsert :: Database -> IO ()
    onInsert = Ref.writeIORef ref

    onGet :: Maybe Db.Value -> IO ()
    onGet = putStrLn . maybe "Key not found" Db.printValue

    onExit :: IO ()
    onExit = exitSuccess

repl :: (String -> IO ()) -> IO ()
repl process =
  prompt *> getLine >>= process

prompt :: IO ()
prompt = T.putStr "Δ. "
