{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, OverloadedStrings #-}

module TSQuery.Query where

import           Prelude hiding ((.))
import           Control.Category

import           Data.Bson
import qualified Data.Text                    as T

data Entity a b = Entity [T.Text] T.Text deriving Show

instance Category Entity where
  id = Entity [] ""
  (Entity b bc) . (Entity a ab) = Entity (a ++ b)
                                         (ab `T.append` "." `T.append` bc)

(.+) :: Entity a b -> Entity b c -> Entity a c
(Entity a ab) .+ (Entity b bc) = Entity (a ++ b)
                                        (ab `T.append` "." `T.append` bc)

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
