module GitUrl where

import Prelude
import Types (IO)
import AurJson (url)
import SinglePackage (single)

import Data.Foldable (traverse_)
import Data.Either (either)

import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (throwException, message)

gitUrl :: String -> IO Unit
gitUrl p = runAff throwException pure do
  res <- single p
  either (liftEff <<< log <<< message) logResult res
    where
    logResult [] = liftEff $ log "No such package found."
    logResult xs = liftEff $ traverse_ (log <<< pprint) xs
    pprint _ = url <> "/" <> p <> ".git"
