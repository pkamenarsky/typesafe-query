{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, OverloadedStrings #-}

module TSQuery.Query where

import           Prelude hiding ((.))
import           Control.Category

import qualified Data.Text                    as T

data Entity a b = Entity T.Text deriving Show

instance Category Entity where
  id = Entity ""
  (Entity a) . (Entity b) = Entity (b `T.append` "." `T.append` a)

(.+) :: Entity a b -> Entity b c -> Entity a c
(Entity a) .+ (Entity b) = Entity (a `T.append` "." `T.append` b)

showEntity :: Entity a b -> T.Text
showEntity (Entity text) = text

class CQuery q v where
  call :: q a
  ceq  :: Eq v  => Entity a v   -> v -> q a
  cneq :: Eq v  => Entity a v   -> v -> q a
  cgrt :: Ord v => Entity a v   -> v -> q a
  cls  :: Ord v => Entity a v   -> v -> q a
  cin  :: Ord v => Entity a [v] -> v -> q a
  cand :: q a -> q a -> q a
  cor  :: q a -> q a -> q a

data Query a =        QAll
 | forall b. Eq b  => QEq (Entity a b) b
 | forall b. Eq b  => QNEq (Entity a b) b
 | forall b. Ord b => QGrt (Entity a b) b
 | forall b. Ord b => QLs (Entity a b) b
 | forall b. Eq b  => QCnt (Entity a [b]) b
 |                    QOr (Query a) (Query a)
 |                    QAnd (Query a) (Query a)
 |                    QNot (Query a)

eq :: Eq b => Entity a b -> b -> Query a
eq = QEq

grt :: Ord b => Entity a b -> b -> Query a
grt = QGrt

ls :: Ord b => Entity a b -> b -> Query a
ls = QLs

contains :: Eq b => Entity a [b] -> b -> Query a
contains = QCnt

qall :: Query a
qall = QAll

qor :: Query a -> Query a -> Query a
qor = QOr

qand :: Query a -> Query a -> Query a
qand = QAnd

qnot :: Query a -> Query a
qnot = QNot
