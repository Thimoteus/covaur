module Search where

import Prelude
import AurJson (aurl)
import Types (IO, Result(..))
import Util (printResult, lpad, green)

import Data.Foldable (traverse_, foldMap)
import Data.Either (either)

import Node.SimpleRequest.Secure (get)

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)

search :: String -> IO Unit
search p = launchAff do
  res <- get $ aurl <> "&type=search&arg=" <> p
  either (liftEff <<< log) logResult $ printResult res.body
    where
    logResult [] = liftEff $ log "No such package found."
    logResult xs = liftEff $ traverse_ (log <<< pprint) xs
    pprint (Result r) =
      foldMap (_ <> "\n") [ green r.name <> "@" <> r.version
                          , lpad r.description 4
                          ]
