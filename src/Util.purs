module Util where

import Prelude

import Data.Either (Either(..))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, catchException)

catch :: forall a eff. Eff ( err :: EXCEPTION | eff ) a -> Eff eff (Either Error a)
catch e = catchException (pure <<< Left) (e >>= pure <<< Right)

