module Search where

import Prelude
import AurJson (aurl)
import Types (IO)
import Util (logAnything)

import Node.SimpleRequest.Secure (get)

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)

search :: String -> IO Unit
search p = launchAff do
  res <- get $ aurl <> "&type=search&arg=" <> p
  --let searchResults = 
  liftEff $ logAnything res.body --logAnything res.body
