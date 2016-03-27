module Info where

import Prelude
import Types (IO, Result(..))
import AurJson (url)
import Util (showDate, rpadTil, showStrArrayPad)
import SinglePackage (single)

import Data.Foldable (traverse_, foldMap)
import Data.Either (either)
import Data.Maybe (Maybe(Just))

import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (throwException, message)

info :: String -> IO Unit
info p = runAff throwException pure do
  res <- single p
  either (liftEff <<< log <<< message) logResult res
    where
    logResult [] = liftEff $ log "No such package found."
    logResult xs = liftEff $ traverse_ (log <<< pprint) xs
    f = flip rpadTil 23
    g = showStrArrayPad 23
    pprint (Result r) =
      foldMap (_ <> "\n") [ f "Name: " <> r.name
                          , f "Package Base: " <> r.packageBase
                          , f "Version: " <> r.version
                          , f "Description: " <> r.description
                          , f "Source: " <> r.url
                          , f "Votes: " <> show r.numVotes
                          , f "Popularity: " <> show r.popularity
                          , f "Maintainer: " <> r.maintainer
                          , f "First submitted: " <> showDate r.firstSubmitted
                          , f "Last modified: " <> showDate r.lastModified
                          , f "Snapshot file: " <> url <> r.urlPath
                          , f "Depends on: " <> g r.depends
                          , f "Make depends on: " <> g r.makeDepends
                          , f "Optional dependencies: " <> g r.optDepends
                          , f "Conflicts: " <> g r.conflicts
                          , f "Provides: " <> g r.provides
                          , f "License: " <> g r.license
                          , f "Keywords: " <> g r.keywords
                          ] <> case r.outOfDate of
                                    Just d -> "Out of date: " <> showDate d
                                    _ -> ""
