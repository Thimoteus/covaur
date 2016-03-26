module Main where

import Prelude
import Control.Monad.Eff.Console (log)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Node.Yargs.Setup as Setup
import Node.Yargs.Applicative as Yargs

import Search (search)
import Info (info)
import GitUrl (gitUrl)
import Util (catch, printMessage)
import Types (IO, App)

import Partial.Unsafe (unsafePartial)

type IsSearch = Boolean
type IsInfo = Boolean
type IsGitUrl = Boolean
type Package = String

covaur :: Partial => Package -> IsSearch -> IsInfo -> IsGitUrl -> IO Unit
covaur package true _ _ = search package
covaur package _ true _ = info package
covaur package _ _ true = gitUrl package

usage :: String
usage = "[-p|--package PACKAGE] [-s|--search] [-i|--info] [-g|--git-url] [--help]"

main :: App
main = do
  res <- catch cli
  case res of
       Left e -> do
         printMessage e
         log "If you believe this to be a bug, open an issue at https://www.github.com/Thimoteus/covaur/issues/"
         log $ "Usage: covaur " <> usage
       _ -> pure unit
  where
  packageArg = Yargs.yarg "p" ["package"] (Just "An AUR package name") (Right "Supply the name of an AUR package") false
  searchArg = Yargs.flag "s" ["search"] (Just "Search the AUR for packages")
  infoArg = Yargs.flag "i" ["info"] (Just "Get AUR info on a package")
  gitUrlArg = Yargs.flag "g" ["git-url"] (Just "Prints a package's git repository address")
  setup = Setup.usage ("covaur " <> usage)
       <> Setup.help "help" "prints this message"
       <> Setup.example "covaur -sp purescript-bin" "Searches the AUR for a package named `purescript-bin` and prints the results"
       <> Setup.example "git clone $(covaur -gp purescript-bin)" "Clones the `purescript-bin` repository"
  cli = Yargs.runY setup $ (unsafePartial covaur) <$> packageArg <*> searchArg <*> infoArg <*> gitUrlArg

