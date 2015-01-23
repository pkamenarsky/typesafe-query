{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, OverloadedStrings #-}

module TSQuery.Query where

import           Prelude hiding ((.))
import           Control.Category

import           Data.Bson
import qualified Data.Text                    as T

data Entity a b = Entity T.Text deriving Show

instance Category Entity where
  id = Entity ""
  (Entity a) . (Entity b) = Entity (b `T.append` "." `T.append` a)

(.+) :: Entity a b -> Entity b c -> Entity a c
(Entity a) .+ (Entity b) = Entity (a `T.append` "." `T.append` b)

showEntity :: Entity a b -> T.Text
showEntity (Entity text) = text

data Query a =                 QAll
 | forall b. (Val b, Eq  b) => QEq  (Entity a b) b
 | forall b. (Val b, Eq  b) => QNeq (Entity a b) b
 | forall b. (Val b, Ord b) => QGrt (Entity a b) b
 | forall b. (Val b, Ord b) => QLs  (Entity a b) b
 | forall b. (Val b, Eq  b) => QCnt (Entity a [b]) b
 |                             QOr  (Query a) (Query a)
 |                             QAnd (Query a) (Query a)
 |                             QNot (Query a)

eq :: (Val b, Eq b) => Entity a b -> b -> Query a
eq = QEq

neq :: (Val b, Eq b) => Entity a b -> b -> Query a
neq = QNeq

grt :: (Val b, Ord b) => Entity a b -> b -> Query a
grt = QGrt

ls :: (Val b, Ord b) => Entity a b -> b -> Query a
ls = QLs

contains :: (Val b, Eq b) => Entity a [b] -> b -> Query a
contains = QCnt

qall :: Query a
qall = QAll

qor :: Query a -> Query a -> Query a
qor = QOr

qand :: Query a -> Query a -> Query a
qand = QAnd

qnot :: Query a -> Query a
qnot = QNot
