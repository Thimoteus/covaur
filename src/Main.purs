module Main where

import Prelude
import Control.Monad.Eff.Console (log)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.String (null)

import Node.Yargs.Setup as Setup
import Node.Yargs.Applicative as Yargs

import Search (search)
import Info (info)
import GitUrl (gitUrl)
import Util (catch, printMessage)
import Types (IO, App)

import Partial.Unsafe (unsafePartial)

type IsSearch = String
type IsInfo = String
type IsGitUrl = String

covaur :: Partial => IsSearch -> IsInfo -> IsGitUrl -> IO Unit
covaur s _ _ | not (null s) = search s
covaur _ s _ | not (null s) = info s
covaur _ _ s | not (null s) = gitUrl s

usage :: String
usage = " [-s|--search PACKAGE] [-i|--info PACKAGE] [-g|--git-url PACKAGE] [--help]"

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
  searchArg = Yargs.yarg "s" ["search"] (Just "Search the AUR for packages") (Left "") true
  infoArg = Yargs.yarg "i" ["info"] (Just "Get AUR info on a package") (Left "") false
  gitUrlArg = Yargs.yarg "g" ["git-url"] (Just "Prints a package's git repository address") (Left "") true
  setup = Setup.usage ("covaur " <> usage)
       <> Setup.help "help" "prints this message"
       <> Setup.example "covaur -s purescript-bin" "Searches the AUR for a package named `purescript-bin` and prints the results"
       <> Setup.example "git clone $(covaur -g purescript-bin)" "Clones the `purescript-bin` repository"
  cli = Yargs.runY setup $ (unsafePartial covaur) <$> searchArg <*> infoArg <*> gitUrlArg

