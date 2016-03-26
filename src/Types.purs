module Types where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Monoid (class Monoid, mempty)
import Data.Foreign (Foreign, F)
import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Foreign.NullOrUndefined (readNullOrUndefined, runNullOrUndefined)
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
         , outOfDate :: Boolean
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
    name <- safeToValue "Name" value
    packageBaseID <- readProp "PackageBaseID" value
    packageBase <- safeToValue "PackageBase" value
    version <- safeToValue "Version" value
    description <- safeToValue "Description" value
    url <- readProp "URL" value
    numVotes <- readProp "NumVotes" value
    popularity <- readProp "Popularity" value
    outOfDate <- safeWithValue false "OutOfDate" value
    maintainer <- safeToValue "Maintainer" value
    firstSubmitted <- toDate <$> readProp "FirstSubmitted" value
    lastModified <- toDate <$> readProp "LastModified" value
    urlPath <- readProp "URLPath" value
    depends <- safeToValue "Depends" value
    makeDepends <- safeToValue "MakeDepends" value
    optDepends <- safeToValue "OptDepends" value
    conflicts <- safeToValue "Conflicts" value
    provides <- safeToValue "Provides" value
    license <- safeToValue "License" value
    keywords <- safeToValue "Keywords" value
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
    results <- safeToValue "results" value
    pure $ Search resultcount results

safeToValue :: forall m. (Monoid m, IsForeign m) => String -> Foreign -> F m
safeToValue = safeWithValue mempty

safeWithValue :: forall a. IsForeign a => a -> String -> Foreign -> F a
safeWithValue safe key val = do
  v <- readProp key val
  u <- readNullOrUndefined read v
  pure $ fromMaybe safe $ runNullOrUndefined u
