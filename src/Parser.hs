module Parser
  ( parseValue,
    parseKey,
    parseCommand,
    Command (..),
  )
where

import Data.Functor (void, ($>))
import Data.Void (Void)
import Database (Index (Index), Key, Value (..), mkKey)
import Eval
import Text.Megaparsec (Parsec, many, noneOf, parseError, (<|>))
import Text.Megaparsec.Char (char, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Parser = Parsec Void String

parseCommand :: Parser Command
parseCommand = parseExit <|> parseInsert <|> parseGet
  where
    parseExit :: Parser Command
    parseExit = string "exit" $> Exit

    parseInsert :: Parser Command
    parseInsert = do
      void $ string "insert "
      key <- parseKey
      space
      value <- parseValue
      pure $ Insert key value

    parseGet :: Parser Command
    parseGet = do
      void $ string "get "
      key <- parseKey
      parseGetIndex key <|> pure (Get key)

    parseGetIndex :: Key -> Parser Command
    parseGetIndex key = do
      space
      GetIndex key . Index <$> decimal

parseKey :: Parser Key
parseKey = do
  key <- many (noneOf ['"', ' ', '\n'])
  case mkKey key of
    Nothing -> parseError mempty
    Just k -> pure k

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
    parseText = VText <$> parseRawText

    parseRawText :: Parser String
    parseRawText = do
      void $ char '"'
      t <- many (noneOf ['"'])
      void $ char '"'
      pure t

    parseInt :: Parser Value
    parseInt = VInt <$> signed space decimal
