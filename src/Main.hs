module Main where

import Control.Applicative ((<$>))
import Data.List (intersperse)
import Data.Traversable (sequenceA)
import Hopts.Language
import Hopts.Options
import Options.Applicative (execParser, info, fullDesc)
import System.Environment (getArgs, setEnv)
import Text.ParserCombinators.Parsec (parse)

setEnvVar :: (EnvVar,String) -> IO ()
setEnvVar (envVar, str) = setEnv envVar str

main :: IO ()
main = do
  argString <- foldr (++) " " <$> intersperse " " <$> getArgs
  let args = case parse parseExpression "(hopts)" argString of
                  Left _ -> error $ "unable to parse argument string: " ++ argString
                  Right as -> as
  pairs <- execParser $ flip info fullDesc $ sequenceA $ map argToOption args
  mapM_ setEnvVar pairs
