{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import Data.Convertible
import Data.Convertible.Instances.Time

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
lt      = EOp Lt
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

{-# INLINE ts2int #-}
ts2int :: UTCTime -> Int
ts2int x =
  case safeConvert x of
    Right y -> y
    Left err -> error "timestamp conversion is always defined"

vcompose :: Val -> Val -> Val
vcompose (ICmp f) (ICmp g) = ICmp (\x -> f x && g x)
vcompose (IUnary f) (IUnary g) = IUnary (\x -> f (g x))
vcompose (IUnary f) (IBinary g) = IBinary (\x y -> f (g x y))

vexpr :: Expr -> Val
vexpr (EOp Gt ETime (ETimestamp x)) = ICmp (> ts2int x)
vexpr (EOp Lt ETime (ETimestamp x)) = ICmp (< ts2int x)
vexpr (EOp And a b) = vcompose (vexpr a) (vexpr b)

vstmt :: Stmt -> V.Vector Int -> V.Vector Int
vstmt (Select col src p) vec = V.filter (icmp (vexpr p)) vec

icmp :: Val -> (Int -> Bool)
icmp (ICmp f) = f

iun :: Val -> (Int -> Int)
iun (IUnary f) = f

ibin :: Val -> (Int -> Int -> Int)
ibin (IBinary f) = f

test :: V.Vector Int
test = vstmt expr (V.fromList [1..200])

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
