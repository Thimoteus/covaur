module Util where

import Prelude
import Types (Result, resultsFromSearch)

import Data.Either (Either(..), either)
import Data.Date (Date)
import Data.String (length, fromCharArray)
import Data.Array (replicate, drop, take)
import Data.Foldable (intercalate)
import Data.Foreign (Foreign)
import Data.Foreign.Class (readJSON)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, catchException, message)
import Control.Monad.Eff.Console (CONSOLE, log)

import Ansi.Codes (EscapeCode(Graphics), GraphicsParam(PMode, PForeground, Reset), RenderingMode(Bold), escapeCodeToString, Color(Green))
import Unsafe.Coerce (unsafeCoerce)

catch :: forall a eff. Eff ( err :: EXCEPTION | eff ) a -> Eff eff (Either Error a)
catch e = catchException (pure <<< Left) (e >>= pure <<< Right)

printMessage :: forall eff. Error -> Eff ( console :: CONSOLE | eff ) Unit
printMessage = log <<< message

foreign import logAnything :: forall a eff. a -> Eff ( console :: CONSOLE | eff ) Unit

foreign import spy :: forall a. a -> a

foreign import showDate :: Date -> String

rpadTil :: String -> Int -> String
rpadTil s l | length s <= l = append s $ fromCharArray $ replicate (l - length s) ' '
rpadTil s _ = s

lpad :: String -> Int -> String
lpad s l = (_ <> s) $ fromCharArray $ replicate l ' '

showStrArrayPad :: Int -> Array String -> String
showStrArrayPad _ [] = ""
showStrArrayPad _ [x] = x
showStrArrayPad l xs = z where
  paddedTail = flip lpad l <$> tail xs
  z = intercalate "\n" $ take 1 xs <> paddedTail

showStrArray :: Array String -> String
showStrArray = showStrArrayPad 0

tail :: forall a. Array a -> Array a
tail = drop 1

printResult :: Foreign -> Either String (Array Result)
printResult val = either (Left <<< show) (Right <<< resultsFromSearch) $ readJSON $ unsafeCoerce val

bolden :: String -> String
bolden s = escapeCodeToString (Graphics [PMode Bold])
        <> s
        <> escapeCodeToString (Graphics [Reset])

green :: String -> String
green s = escapeCodeToString (Graphics [PForeground Green])
       <> s
       <> escapeCodeToString (Graphics [Reset])
