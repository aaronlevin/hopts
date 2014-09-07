{-| This module contains methods to transform 'ArgData' into types provided by the 'optparse-applicative' library.
-}

module Hopts.Options (
  argDataToOptParse
  ) where

import Control.Applicative ((<$>))
import Hopts.Language (ArgData(ShortArg))
import Options.Applicative ((<>), help, Parser, short, switch, strOption)

{-| 'argDataToOptParse' transforms argument data ('ArgData') into a tuple containing the
environment variable to be set after succesfully parsing a command-line argument via optparse-applicative,
and the value of said data. For example, given an argument data such as:

@
  ShortArg \'x\' \"X_VAR\" \"set the x var\" --> Parser (String, String)
@

upon initializing our application with: '.\/app.sh -x cool' this tuple will represent '(X_VAR, cool)'
-}
argDataToOptParse :: ArgData -> Parser (String, String)
argDataToOptParse (ShortArg argChar envVar quote isSwitch) = 
  (,) envVar <$> (strOption) (short argChar <> help quote)
