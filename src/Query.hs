{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Query (
  Expr(..),
  Stmt(..),
  Name(..),
) where

import Prelude hiding (and)

import Data.Data
import Data.String
import Data.Monoid
import Data.Typeable

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime

import qualified Data.Vector.Storable as V

{-import System.Locale-}

import GHC.Generics

import qualified Data.ByteString as BS

newtype Name = Name BS.ByteString
  deriving (Eq, Ord, Show, IsString, Read, Data, Typeable, Generic, Monoid)

data Stmt
  = Select Expr Expr Expr
  | Append
  deriving (Eq, Ord, Show)

data Expr
  = EName Name
  | ETime
  | EOp Op Expr Expr
  | EStar
  | ETimestamp UTCTime
  deriving (Eq, Ord, Show)

data Op
  = Gt
  | Lt
  | Eq
  | And
  | InRange
  deriving (Eq, Ord, Show)

gt      = EOp Gt
lt      = EOp Gt
eq      = EOp Eq
inRange = EOp InRange
and     = EOp And

data Val
  = ICmp (Int -> Bool)
  | IUnary (Int -> Int)
  | IBinary (Int -> Int -> Int)

-------------------------------------------------------------------------------
-- Evaluator
-------------------------------------------------------------------------------

vcompose :: Val -> Val -> Val
vcompose (ICmp f) (ICmp g) = ICmp (\x -> f x && g x)
vcompose (IUnary f) (IUnary g) = IUnary (\x -> f (g x))
vcompose (IUnary f) (IBinary g) = IBinary (\x y -> f (g x y))

vselect :: Expr -> Val
vselect (EOp Gt ETime (ETimestamp t1)) = ICmp (> t1')
  where
    t1' :: Int
    t1' = undefined (utcTimeToPOSIXSeconds t1)
vselect (EOp And a b) = vcompose (vselect a) (vselect b)

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

t1 :: UTCTime
t1 = parseTimeOrError True defaultTimeLocale "%c" "Thu Jan  1 00:00:10 UTC 1970"

t2 :: UTCTime
t2 = parseTimeOrError True defaultTimeLocale "%c" "Thu Jan  1 00:00:10 UTC 2000"

expr :: Stmt
expr =
  Select (EName "accounts") (EName "data")
    ((ETime `gt` (ETimestamp t1))
        `and`
     (ETime `lt` (ETimestamp t2)))


{-

select value from response_times
where time > '2013-08-12 23:32:01.232' and time < '2013-08-13';

-}
