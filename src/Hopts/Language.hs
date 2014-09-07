module Hopts.Language (
    ArgData(..)
  , ArgCommand(..)
  , parseArgCmd
  , parseArgDataExpression
  , parseEnvVar
  , parseExpression
  , parseExpressions
  , parseLongArg
  , parseLongArgData
  , parseQuote
  , parseShortArgData
  , parseShortLongArgData
  , parseTransformerExpression
  ) where

import Control.Applicative ((*>), (<*))
import Text.ParserCombinators.Parsec ((<|>), between, char, letter, many, noneOf, Parser, try, sepBy, spaces, upper)

{-| 'ArgCommand' represents the various commands in our language
-}
data ArgCommand = ShortCmd     -- ^ 'short'
                | LongCmd      -- ^ 'long'
                | ShortLongCmd -- ^ 'short-long'

{-| 'ArgData' describes Data contained in command-line arguments
-}
data ArgData = ShortArg { sArg    :: Char   -- ^ Character representing the argument
                        , sEnv    :: String -- ^ Environment variable to be set
                        , sQuote  :: String -- ^ Help string
                        , sSwitch :: Bool   -- ^ 'true' if this is a switch argument
                        }
             | LongArg { lArg    :: String -- ^ argument name (e.g. '"pizza"' = --pizza)
                       , lEnv    :: String -- ^ Environment variable to be set
                       , lQuote  :: String -- ^ Help string
                       , lSwitch :: Bool   -- ^ 'true' if this is a switch argument
                       }
             | ShortLongArg { slArgShort :: Char   -- ^ character representing the argument
                            , slArgLong  :: String -- ^ long, double-dash argument name.
                            , slEnv      :: String -- ^ Environment variable to be set.
                            , slQuote    :: String -- ^ Help string
                            , slSwitch   :: Bool   -- ^ 'true' if this is a switch argument
                            }

{-| 'ArgTransformer' represents transformations we can do to arguments
-}
data ArgTransformer = Switch -- ^ turn an argument into a boolean switch

{-| given a transformer and some argumetn data, perform the transformation
TODO: refactor this to avoid combinatorial explosion down the road
-}
argDataTransformer :: ArgTransformer -> ArgData -> ArgData
argDataTransformer Switch arg@(ShortArg _ _ _ s)     = arg { sSwitch = not s }
argDataTransformer Switch arg@(LongArg _ _ _ s)      = arg { lSwitch = not s }
argDataTransformer Switch arg@(ShortLongArg _ _ _ _ s) = arg { slSwitch = not s }

{-| 'parseShortArgChar' parses a short argument, which is just a character
-}
parseShortArgChar :: Parser Char
parseShortArgChar = letter

{-| 'parseLongArg' parses a long argument, a character string that may 
    contain dashes
 -}
parseLongArg :: Parser String
parseLongArg = many (letter <|> char '-')

{-| 'parseEnvVar' parses the name of an Environment Variable, which is
    assumed to be an upper-case string with underscores
-}
parseEnvVar :: Parser String
parseEnvVar = many (upper <|> char '_')

{-| 'parseQuote' parses help string quotes
-}
parseQuote :: Parser String
parseQuote = char '"' *> many (noneOf "\"") <* char '"'

{-| 'parseArgCmd' parses an argument command. Example arg-commands:
    1. 'short'
    2. 'long'
    3. 'short-long'
-}
parseArgCmd :: Parser ArgCommand
parseArgCmd = do
  cmd <- many (letter <|> char '-')
  case cmd of
    "short"      -> return ShortCmd
    "long"       -> return LongCmd
    "short-long" -> return ShortLongCmd
    _            -> fail ("unknown argument command: " ++ cmd)

{-| 'parseTransformerCmd' parses a transformer command. Example transformer commands:
    1. 'switch'
-}
parseTransformerCmd :: Parser ArgTransformer
parseTransformerCmd = do
  cmd <- many (letter <|> char '-')
  case cmd of
    "switch" -> return Switch
    _        -> fail ("unknown transformer command: " ++ cmd)

{-| 'parseShortArgData' parses elements occuring after a short argument command
@
  (short x X_VAR "set x")'
         ^_____________^
                v
         parsing this part
@
-}
parseShortArgData :: Parser ArgData
parseShortArgData = do
  shortArg <- parseShortArgChar
  envVar <- spaces *> parseEnvVar <* spaces
  quote <- parseQuote
  return $ ShortArg shortArg envVar quote False

{-| 'parseLongArgData' parses elements occuring after a long argument command
@
  (long xxyyzz XYZ_VAR "set XYZ_VAR to value")
        ^___________________________________^
                         v
                 parsing this part
@
-}
parseLongArgData :: Parser ArgData
parseLongArgData = do
  longArg <- parseLongArg
  envVar <- spaces *> parseEnvVar <* spaces
  quote <- parseQuote
  return $ LongArg longArg envVar quote False

{-| 'parseShortLongArgCmd' parses elements occuring after a short-long command
@
  (short-long a aws-key AWS_KEY "Your amazon key")
              ^_________________________________^
                              v
                      parsing this part
@
-}
parseShortLongArgData :: Parser ArgData
parseShortLongArgData = do
  shortArg <- parseShortArgChar
  spaces
  longArg <- parseLongArg
  spaces
  envVar <- parseEnvVar
  spaces
  quote <- parseQuote
  return $ ShortLongArg shortArg longArg envVar quote False

{-| 'parseArgDataExpression' parses an argument expression within brackets
@
  (short a AWS_KEY "your aws key")
  ^______________________________^
               v
        parsing this part
@
-}
parseArgDataExpression :: Parser ArgData
parseArgDataExpression = do
  _ <- char '('
  argCmd <- parseArgCmd
  spaces
  argData <- case argCmd of
    ShortCmd -> parseShortArgData
    LongCmd -> parseLongArgData
    ShortLongCmd -> parseShortLongArgData
  spaces
  _ <- char ')'
  return argData

{-| 'parseTransformerExpression' parsers a transformer expression
@
  (switch (short-long a aws-key AWS_KEY "your aws key"))
  ^____________________________________________________^
                          v
                  parsing this part
@
-}
parseTransformerExpression :: Parser ArgData
parseTransformerExpression = do
  _ <- char '('
  spaces
  argTransformer <- parseTransformerCmd
  spaces
  argData <- parseArgDataExpression
  spaces
  _ <- char ')'
  return $ argDataTransformer argTransformer argData

{-| 'parseExpression' parses the most general, bracketed s-expression
@
  (switch (...))
  ^____________^
        v
      (this)
@
or
@
  (short-long ...)
  ^______________^
         v
       (this)
@
-}
parseExpression :: Parser ArgData
parseExpression = do
  _ <- char '('
  spaces
  argData <- try parseTransformerExpression <|> parseArgDataExpression
  spaces
  _ <- char '('
  return argData


{-| 'parseExpressions' parses a list of expressions, wrapped in square brackets
@
  [ (...arg1...) , (...arg2...) , ... ]
  ^___________________________________^
                  v
                (this)
@
results in '[arg1, arg2, ...]'
-}
parseExpressions :: Parser [ArgData]
parseExpressions = do
  _ <- char '['
  spaces
  args <- between spaces spaces parseExpression `sepBy` char ','
  spaces
  _ <- char ']'
  return args
