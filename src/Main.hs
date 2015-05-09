{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Data
import Data.String
import Data.Serialize
import qualified Data.Map as Map

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Vector.Storable.ByteString (vectorToByteString, byteStringToVector)
import qualified Data.Vector.Storable as V

import Control.Exception

import GHC.Generics

import Data.Monoid

import Data.Convertible
import Data.Convertible.Instances.Time

import Control.Applicative
import Control.Monad (when)

import System.FilePath
import System.Directory

import System.IO.Posix.MMap
import System.IO.Posix.MMap.Internal

import Query

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Metadata = Metadata
  { schema :: Schema
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Db = Db
  { metadata :: Metadata
  , directory :: FilePath
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data ColType
  = TInt
  | TFloat
  | TText
  deriving (Eq, Ord, Show, Read, Enum, Data, Typeable, Generic)

instance Serialize Db
instance Serialize ColType
instance Serialize Name
instance Serialize Metadata

type Schema = Map.Map Name ColType

-------------------------------------------------------------------------------
-- Access
-------------------------------------------------------------------------------

readDb :: FilePath -> IO (Either String Db)
readDb dir = do
  exists <- doesDirectoryExist dir
  if exists
  then do
    md <- decode <$> BS.readFile (dir </> "meta")
    return (Db <$> md <*> pure dir)
  else
    return (Left "No such directory")

writeDb :: Db -> IO ()
writeDb db = do
  exists <- doesDirectoryExist (directory db)
  when (not exists) $ createDirectory (directory db)
  BS.writeFile ((directory db) </> "meta") (encode (metadata db))

exammpleDb :: Db
exammpleDb = Db
  { metadata  = Metadata { schema = Map.fromList [("balancce", TInt)] }
  , directory = "data"
  }

-------------------------------------------------------------------------------
-- Column
-------------------------------------------------------------------------------

toColPath :: Name -> FilePath
toColPath (Name nm) = B8.unpack nm

writeCol :: V.Storable a => Db -> Name -> V.Vector a -> IO ()
writeCol db col vec = do
  BS.writeFile ((directory db) </> toColPath col) (vectorToByteString vec)

{-# NOINLINE readCol #-}
readCol :: (V.Storable a, V.Storable b) => Db -> Name -> IO (V.Vector a, V.Vector b)
readCol db col = do
  {-bs <- BS.readFile ((directory db) </> toColPath col)-}
  ts   <- unsafeMMapFile ((directory db) </> toColPath col <> "__ts")
  vals <- unsafeMMapFile ((directory db) </> toColPath col <> "__data")
  return (byteStringToVector ts, byteStringToVector vals)

writeTs :: (V.Storable a, V.Storable b) => Db -> Name -> V.Vector a -> V.Vector b -> IO ()
writeTs db col ts vals = do
  writeCol db (col <> "__ts") ts
  writeCol db (col <> "__data") vals

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

main :: IO ()
main = do
  writeDb exammpleDb
  mdb <- readDb "data"
  case mdb of
    Left err -> print err
    Right db -> do
      rc <- writeTs db "accounts" ([1..100] :: V.Vector Int) ([1..100] :: V.Vector Int)
      col <- readCol db "accounts"
      print (col :: (V.Vector Int, V.Vector Int))
      return ()
