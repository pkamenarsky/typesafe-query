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

data Query a op =       QAll
 | forall b. (Val b) => QBin op (Entity a b) b
 |                      QOr  (Query a op) (Query a op)
 |                      QAnd (Query a op) (Query a op)
 |                      QNot (Query a op)

qall :: Query a op
qall = QAll

qor :: Query a op -> Query a op -> Query a op
qor = QOr

qand :: Query a op -> Query a op -> Query a op
qand = QAnd

qnot :: Query a op -> Query a op
qnot = QNot
