{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module TSQuery.TH where

import           Language.Haskell.TH

import           Control.Monad

import           Data.List
import qualified Data.Text                    as T

import           TSQuery.Query

mkTSEntities :: Bool -> Name -> Q [Dec]
mkTSEntities _literal tyName = do
  info <- reify tyName
  case info of
    TyConI (DataD _ _ _ cons _)
      | length cons > 255 -> fail $ "Can't derive entities for: " ++ show tyName ++
                                    ". The datatype must have less than 256 constructors."
      | otherwise         -> worker cons
    TyConI (NewtypeD _ _ _ con _) ->
      worker [con]
    FamilyI _ insts -> do
      decs <- forM insts $ \inst ->
        case inst of
          DataInstD _ _ _ cons _ ->
              worker cons
          NewtypeInstD _ _ _ con _ ->
              worker [con]
          _ -> fail $ "Can't derive entities instance for: " ++ show (tyName, inst)
      return $ concat decs
    _ -> fail $ "Can't derive entities instance for: " ++ show (tyName, info)
  where
    worker cons = sequence $ mkFields cons
    cFields = map (\(name, _, _) -> mkName $ (nameBase name))
    prfField name = mkName $ "_" ++ (nameBase name)
    mkField name = valD (varP $ prfField name) (normalB $ conE 'Entity `appE` ((varE 'T.pack) `appE` (litE $ stringL $ nameBase name))) []
    mkFields cons = map mkField $ nub [ field | (RecC _ fields) <- cons, field <- cFields fields ]
