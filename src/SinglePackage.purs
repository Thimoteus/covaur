module SinglePackage where

import Prelude
import Types (Result)
import AurJson (aurl)
import Util (printResult)

import Data.Either (Either(..), either)

import Control.Monad.Eff.Exception (Error, error)

import Node.SimpleRequest (AffReq)
import Node.SimpleRequest.Secure (get)

single :: forall eff. String -> AffReq eff (Either Error (Array Result))
single p = do
  res <- get $ aurl <> "&type=info&arg[]=" <> p
  pure $ either (Left <<< error) Right $ printResult res.body 
