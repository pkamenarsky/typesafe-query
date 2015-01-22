{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}

module TSQuery.Query where

import           Control.Category

import qualified Data.Text                    as T

data Entity a b = Entity T.Text deriving Show

instance Category Entity where
  id = Entity ""
  (Entity a) . (Entity b) = Entity (a `T.append` "." `T.append` b)

data Query a = forall b. Eq b  => QEq (Entity a b) b
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

or :: Query a -> Query a -> Query a
or = QOr

select :: Query a -> a
select = undefined
