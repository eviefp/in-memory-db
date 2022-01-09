module Parser
  ( parseValue,
    parseKey,
    parseCommand,
    Command (..),
  )
where

import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Database (Key, Value (..), mkKey)
import Text.Megaparsec (Parsec, many, noneOf, parseError, (<|>))
import Text.Megaparsec.Char (char, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Prelude (Monad ((>>=)), Monoid (mempty), maybe, pure, ($), (*>), (.), (<$>))

type Parser = Parsec Void Text

data Command
  = Insert !Key !Value
  | Get !Key

parseCommand :: Parser Command
parseCommand = parseInsert <|> parseGet
  where
    parseInsert :: Parser Command
    parseInsert = do
      _ <- string "insert "
      key <- parseKey
      space
      value <- parseValue
      pure $ Insert key value

    parseGet :: Parser Command
    parseGet = do
      _ <- string "get "
      Get <$> parseKey

parseKey :: Parser Key
parseKey =
  many (noneOf ['"', ' ', '\n'])
    >>= maybe (parseError mempty) pure
      . mkKey
      . T.pack

parseValue :: Parser Value
parseValue = parseList <|> parseText <|> parseInt
  where
    parseList :: Parser Value
    parseList = do
      void $ char '['
      result <- parseTextList <|> parseIntList
      void $ char ']'
      pure result

    parseTextList :: Parser Value
    parseTextList = do
      first <- parseRawText
      rest <- many (char ',' *> parseRawText)
      pure $ VTextList (first : rest)

    parseIntList :: Parser Value
    parseIntList = do
      first <- signed space decimal
      rest <- many (char ',' *> signed space decimal)
      pure $ VIntList (first : rest)

    parseText :: Parser Value
    parseText = do
      VText <$> parseRawText

    parseRawText :: Parser Text
    parseRawText = do
      void $ char '"'
      t <- many (noneOf ['"'])
      void $ char '"'
      pure $ T.pack t

    parseInt :: Parser Value
    parseInt = VInt <$> signed space decimal
