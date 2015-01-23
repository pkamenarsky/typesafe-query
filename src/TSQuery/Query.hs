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
 | forall b. (Val b, Ord b) => QGt  (Entity a b) b
 | forall b. (Val b, Ord b) => QLt  (Entity a b) b
 | forall b. (Val b)        => QBin (Entity a b) b T.Text
 | forall b. (Val b, Eq  b) => QCnt (Entity a [b]) b
 |                             QOr  (Query a) (Query a)
 |                             QAnd (Query a) (Query a)
 |                             QNot (Query a)

eq :: (Val b, Eq b) => Entity a b -> b -> Query a
eq = QEq

neq :: (Val b, Eq b) => Entity a b -> b -> Query a
neq = QNeq

gt :: (Val b, Ord b) => Entity a b -> b -> Query a
gt = QGt

lt :: (Val b, Ord b) => Entity a b -> b -> Query a
lt = QLt

bin :: (Val b) => Entity a b -> b -> T.Text -> Query a
bin = QBin

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
