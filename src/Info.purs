module Info where

import Prelude
import Types (IO, Result(..))
import AurJson (url)
import Util (showDate, rpadTil, showStrArrayPad, green, red, printError)
import SinglePackage (single)

import Data.Foldable (traverse_, foldMap)
import Data.Either (either)
import Data.Maybe (Maybe(Just), isJust)

import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (message)

info :: String -> IO Unit
info p = runAff printError pure do
  res <- single p
  either (liftEff <<< log <<< message) logResult res
    where
    logResult [] = liftEff $ log "No such package found."
    logResult xs = liftEff $ traverse_ (log <<< pprint) xs
    f = flip rpadTil 20
    g = showStrArrayPad 20
    pprint (Result r) =
      foldMap (_ <> "\n") [ (if isJust r.outOfDate then red else green) r.name
                          , f "Package Base:" <> r.packageBase
                          , f "Version:" <> r.version
                          , f "Description:" <> r.description
                          , f "Source:" <> r.url
                          , f "Votes:" <> show r.numVotes
                          , f "Popularity:" <> show r.popularity
                          , f "Maintainer:" <> r.maintainer
                          , f "First submitted:" <> showDate r.firstSubmitted
                          , f "Last modified:" <> showDate r.lastModified
                          , f "Snapshot file:" <> url <> r.urlPath
                          , f "Depends on:" <> g (r.depends <> map (_ <> " (make)") r.makeDepends <> map (_ <> " (opt)") r.optDepends)
                          , f "Conflicts:" <> g r.conflicts
                          , f "Provides:" <> g r.provides
                          , f "License:" <> g r.license
                          , f "Keywords:" <> g r.keywords
                          ] <> case r.outOfDate of
                                    Just d -> red $ f "Out of date:" <> showDate d
                                    _ -> ""
