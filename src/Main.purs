module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Node.Yargs.Setup as Setup
import Node.Yargs.Applicative as Yargs

import Util (catch)

type IsSearch = Boolean
type Package = String

covaur :: forall eff. Package -> IsSearch -> Eff ( console :: CONSOLE | eff ) Unit
covaur package true = log package

usage :: String
usage = "[-p|--package PACKAGE] [-s|--search] [--help]"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  res <- catch cli
  case res of
       Left _ -> log $ "Usage: covaur " <> usage
       _ -> pure unit
  where
  packageArg = Yargs.yarg "p" ["package"] (Just "An AUR package name") (Right "Supply the name of an AUR package") false
  searchArg = Yargs.flag "s" ["search"] (Just "Prints a list of search matches")
  setup = Setup.usage ("$0 " <> usage)
       <> Setup.help "help" "prints this message"
  cli = Yargs.runY setup $ covaur <$> packageArg <*> searchArg

