module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.String (null)

import Node.Yargs.Setup as Setup
import Node.Yargs.Applicative as Yargs

import Search (search)
import Info (info)
import GitUrl (gitUrl)
import Util (catch, printError)
import Types (IO, App)

import Partial.Unsafe (unsafePartial)

type IsSearch = String
type IsInfo = String
type IsGitUrl = String
type ByMaintainer = Boolean
type ByName = Boolean

covaur :: Partial => IsSearch
                  -> IsInfo
                  -> IsGitUrl
                  -> ByName
                  -> ByMaintainer
                  -> IO Unit
covaur s _ _ _ _    | not (null s) = info s
covaur _ s _ _ _    | not (null s) = gitUrl s
covaur _ _ s true _ | not (null s) = search "name" s
covaur _ _ s _ m    | m          = search "maintainer" s
covaur _ _ s _ _    | not (null s) = search "name-desc" s

usage :: String
usage = " [-s|--search PACKAGE] [-i|--info PACKAGE] [-g|--git-url PACKAGE] [-m|--by-maintainer] [-n|--by-name] [--help]"

main :: App
main = do
  res <- catch cli
  case res of
       Left e -> printError e
       _ -> pure unit
  where
  searchArg = Yargs.yarg "s" ["search"] (Just "Search the AUR for packages") (Left "") true
  infoArg = Yargs.yarg "i" ["info"] (Just "Get AUR info on a package") (Left "") true
  gitUrlArg = Yargs.yarg "g" ["git-url"] (Just "Prints a package's git repository address") (Left "") true
  byName = Yargs.flag "n" ["by-name"] (Just "Search by a package's name only")
  byMaintainer = Yargs.flag "m" ["by-maintainer"] (Just "Search by a package's maintainer")
  setup = Setup.usage ("covaur " <> usage)
       <> Setup.help "help" "prints this message"
       <> Setup.version "version" "display the version number" "1.1.0"
       <> Setup.example "covaur --search purescript-bin" "Searches the AUR for a package named `purescript-bin` and prints the results"
       <> Setup.example "git clone $(covaur -g purescript-bin)" "Clones the `purescript-bin` repository"
  cli = Yargs.runY setup $ unsafePartial covaur <$> infoArg
                                                <*> gitUrlArg
                                                <*> searchArg
                                                <*> byName
                                                <*> byMaintainer

