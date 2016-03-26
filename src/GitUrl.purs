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
import Control.Monad.Eff.Exception (throwException)

gitUrl :: String -> IO Unit
gitUrl p = runAff throwException pure do
  res <- single p
  either (const $ pure unit) (liftEff <<< traverse_ logResult) res
    where
    logResult  = log <<< pprint
    pprint _ = url <> "/" <> p <> ".git"
