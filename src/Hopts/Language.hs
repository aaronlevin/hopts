module Hopts.Language (
    ArgFlag
  , ArgType(..)
  , Argument(..)
  , parseArgType
  , parseArgFlag
  , parseEnvVar
  , parseQuote
  , parseArgument
  , parseExpression
  ) where

import Control.Applicative ((*>), (<*))
import Data.These (These(This,That,These))
import Text.ParserCombinators.Parsec ((<|>), between, char, letter, many, many1, noneOf, optionMaybe, Parser, try, sepBy, spaces, upper)

type EnvVar = String

type ArgFlag = These Char String

data ArgType = SwitchArg
             | StringArg

data Argument = Argument ArgType ArgFlag EnvVar (Maybe String)

parseArgType :: Parser ArgType
parseArgType = do
  argType <- many (letter <|> char '-')
  case argType of
    "switch" -> return SwitchArg
    "string" -> return StringArg
    _        -> fail $ "`" ++ argType ++ "` is not a valid argument type"

parseArgFlag :: Parser ArgFlag
parseArgFlag = do
  argChar <- try $ optionMaybe $ letter <* many1 spaces
  argLong <- try $ optionMaybe $ many (letter <|> char '-')
  case (argChar, argLong) of
    (Just c, Just lng)  -> return $ These c lng
    (Just c, Nothing)   -> return $ This c
    (Nothing, Just lng) -> return $ That lng
    _                   -> fail "must specify either or both a short and long argument flag"

{-| 'parseEnvVar' parses the name of an Environment Variable, which is
    assumed to be an upper-case string with underscores
-}
parseEnvVar :: Parser String
parseEnvVar = many (upper <|> char '_')

{-| 'parseQuote' parses help string quotes
-}
parseQuote :: Parser String
parseQuote = char '"' *> many (noneOf "\"") <* char '"'

parseArgument :: Parser Argument
parseArgument = do
  between (char '(' <* spaces) (spaces *> char ')') $ do
    argType <- parseArgType <* many1 spaces
    argFlag <- parseArgFlag <* many1 spaces
    envVar  <- parseEnvVar  <* spaces
    quote   <- try $ optionMaybe $ parseQuote
    return $ Argument argType argFlag envVar quote

{-| 'parseExpression' parses a list of expressions, wrapped in square brackets

@
\  [ (...arg1...) , (...arg2...) , ... ]
  ^___________________________________^
                  v
                (this)
@

results in @[arg1, arg2, ...]@

-}
parseExpression :: Parser [Argument]
parseExpression = do
  _ <- char '['
  spaces
  args <- between spaces spaces parseArgument `sepBy` char ','
  spaces
  _ <- char ']'
  return args
