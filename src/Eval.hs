module Eval
  ( Command (..),
    eval,
  )
where

import Database

data Command
  = Insert !Key !Value
  | Get !Key
  | GetIndex !Key !Index
  | Exit

type OnInsert m = Database -> m ()

type OnGet m = Maybe Value -> m ()

type OnGetIndex m = Maybe Value -> m ()

type OnExit m = m ()

eval ::
  OnInsert m ->
  OnGet m ->
  OnGetIndex m ->
  OnExit m ->
  Database ->
  Command ->
  m ()
eval onInsert onGet onGetIndex onExit db =
  \case
    Insert k v -> onInsert $ insert k v db
    Get k -> onGet $ get k db
    GetIndex k v -> onGetIndex $ getIndex k v db
    Exit -> onExit
