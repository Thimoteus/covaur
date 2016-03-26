module Types where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Foreign (Foreign, F)
import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Foreign.Null (runNull, readNull)
import Data.Foreign.Undefined (readUndefined, runUndefined)
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Date (Date, fromEpochMilliseconds)
import Data.Time (Milliseconds(..))
import Data.Int (toNumber)

import Node.SimpleRequest (REQUEST)

import Partial.Unsafe (unsafeCrashWith)

type Effects eff = ( console :: CONSOLE, request :: REQUEST | eff )

type IO a = Eff (Effects (err :: EXCEPTION)) a

type App = Eff (Effects ()) Unit

data Search = Search Int (Array Result)

instance showSearch :: Show Search where
  show (Search i xs) = "resultcount: " <> show i <> ", results: " <> show xs

resultsFromSearch :: Search -> Array Result
resultsFromSearch (Search _ xs) = xs

newtype Result =
  Result { id :: Int
         , name :: String
         , packageBaseID :: Int
         , packageBase :: String
         , version :: String
         , description :: String
         , url :: String
         , numVotes :: Int
         , popularity :: Number
         , outOfDate :: Maybe Boolean
         , maintainer :: String
         , firstSubmitted :: Date
         , lastModified :: Date
         , urlPath :: String
         , depends :: Array String
         , makeDepends :: Array String
         , optDepends :: Array String
         , conflicts :: Array String
         , provides :: Array String
         , license :: Array String
         , keywords :: Array String
         }

instance showResult :: Show Result where
  show (Result o) = o.description

instance isForeignResult :: IsForeign Result where
  read value = do
    id <- readProp "ID" value
    name <- readProp "Name" value
    packageBaseID <- readProp "PackageBaseID" value
    packageBase <- readProp "PackageBase" value
    version <- readProp "Version" value
    description <- readProp "Description" value
    url <- readProp "URL" value
    numVotes <- readProp "NumVotes" value
    popularity <- readProp "Popularity" value
    outOfDate <- do
      v <- readProp "OutOfDate" value
      n <- readNull read v
      pure $ runNull n
    maintainer <- readProp "Maintainer" value
    firstSubmitted <- toDate <$> readProp "FirstSubmitted" value
    lastModified <- toDate <$> readProp "LastModified" value
    urlPath <- readProp "URLPath" value
    depends <- readProp "Depends" value
    makeDepends <- safeToArray "MakeDepends" value
    optDepends <- safeToArray "OptDepends" value
    conflicts <- safeToArray "Conflicts" value
    provides <- safeToArray "Provides" value
    license <- safeToArray "License" value
    keywords <- safeToArray "Keywords" value
    pure $ Result { id, name, packageBaseID, packageBase, version, description
                  , url, numVotes, popularity, outOfDate, maintainer, firstSubmitted
                  , lastModified, urlPath, depends, makeDepends, optDepends
                  , conflicts, provides, license, keywords
                  }

toDate :: Int -> Date
toDate = f <<< fromEpochMilliseconds <<< Milliseconds <<< (* 1000.0) <<< toNumber
  where
    f (Just d) = d
    f _ = unsafeCrashWith "Could not read date"

instance isForeignSearch :: IsForeign Search where
  read value = do
    resultcount <- readProp "resultcount" value
    results <- readProp "results" value
    pure $ Search resultcount results

safeToArray :: forall a. IsForeign a => String -> Foreign -> F (Array a)
safeToArray key val = do
  v <- readProp key val
  u <- readUndefined read v
  pure $ fromMaybe [] $ runUndefined u
