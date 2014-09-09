module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (void)
import Data.Traversable (sequenceA)
import Hopts.Language
import Hopts.Options
import Options.Applicative ((<>), info, fullDesc, header, help, helper, long, Parser, ParserPrefs(ParserPrefs), progDesc, strOption)
import Options.Applicative.Extra (execParserPure, execParser, ParserResult(Success, Failure), ParserFailure(execFailure))
import System.Process (rawSystem, system)
import System.Posix.Env (putEnv)
import Text.ParserCombinators.Parsec (parse)

parserPrefs :: ParserPrefs
parserPrefs =  ParserPrefs "suffix" False True True 80

setEnvVar :: (EnvVar,String) -> IO ()
-- setEnvVar (envVar, str) = setEnv envVar str
-- setEnvVar (envVar, str) = void $ system ("echo hello && eval \"" ++ envVar ++ "=" ++ str ++ "\" && echo $" ++ envVar)
setEnvVar (envVar, str) = void $ putEnv("XXX=xxxl") >> system "echo $XXX"

data HoptsArgs = HoptsArgs String String

hoptsOptions :: Parser HoptsArgs
hoptsOptions = HoptsArgs <$> strOption (long "args" <> help "argument language")
                         <*> strOption (long "cmdline" <> help "the original program command line")

main :: IO ()
main = do
  (HoptsArgs argString cmdLine) <- execParser opts
  let args = case parse parseExpression "(hopts)" argString of
                  Left _ -> error $ "unable to parse argument string: " ++ argString
                  Right as -> as
  mapM_ (putStrLn . show) args
  let optParser = flip info fullDesc $ sequenceA $ map argToOption args
  -- case execParserPure parserPrefs optParser $ words cmdLine of
  case execParserPure parserPrefs optParser $ words cmdLine of
    Success argPairs -> (print argPairs) >> mapM_ setEnvVar argPairs
    Failure (pFailure) -> putStrLn $ fst . (execFailure pFailure) $ "hopts"
    _            -> fail ":("
  where
    opts = info (helper <*> hoptsOptions)
      ( fullDesc
      <> progDesc "welcome to Hopts!"
      <> header "hopts - a cool program")
