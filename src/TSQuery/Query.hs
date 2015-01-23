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

data QueryExpOp op a =        QAll
 | forall b. (Eq b, Val b) => QBin op (Entity a b) b
 |                            QOr  (QueryExpOp op a) (QueryExpOp op a)
 |                            QAnd (QueryExpOp op a) (QueryExpOp op a)
 |                            QNot (QueryExpOp op a)

qall, (.*) :: QueryExpOp op a
qall = QAll
(.*) = QAll

qor, (.||) :: QueryExpOp op a -> QueryExpOp op a -> QueryExpOp op a
qor   = QOr
(.||) = QOr

qand, (.&&) :: QueryExpOp op a -> QueryExpOp op a -> QueryExpOp op a
qand  = QAnd
(.&&) = QAnd

qnot, (.^) :: QueryExpOp op a -> QueryExpOp op a
qnot = QNot
(.^) = QNot
