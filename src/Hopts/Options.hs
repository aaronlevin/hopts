{-| This module contains methods to transform 'ArgData' into types provided by the 'optparse-applicative' library.
-}

module Hopts.Options (
  argToOption
  ) where

import Control.Applicative ((<$>))
import Data.Monoid (mempty)
import Data.These (These(This, That, These))
import Hopts.Language (Argument(Argument), ArgFlag, ArgType(SwitchArg, StringArg), EnvVar)
import Options.Applicative.Builder.Internal (HasName, Mod)
import Options.Applicative ((<>), help, long, Parser, short, switch, strOption)

argFlagToMod :: HasName f => ArgFlag -> Mod f a
argFlagToMod (This c) = short c
argFlagToMod (That lng) = long lng
argFlagToMod (These c lng) = short c <> long lng

quote :: Maybe String -> Mod f a
quote (Just str) = help str
quote Nothing = mempty

boolString :: Bool -> String
boolString True = "true"
boolString False = "false"

{-| 'argToOption' transforms an 'Argument' into an optparse-applicative-based parser.
-}
argToOption :: Argument -> Parser (EnvVar, String)
argToOption (Argument SwitchArg argFlag envVar mQuote) = (,) envVar <$> (boolString <$> switch (argFlagToMod argFlag <> quote mQuote))
argToOption (Argument StringArg argFlag envVar mQuote) = (,) envVar <$> (strOption (argFlagToMod argFlag <> quote mQuote))
