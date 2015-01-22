{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module TSQuery.TH where

import Language.Haskell.TH

import Control.Applicative
import Control.Monad

import qualified Data.Text                    as T
import Data.Maybe (fromMaybe, listToMaybe)

import TSQuery.Query

mkTSEntities :: Bool -> Name -> Q [Dec]
mkTSEntities literal tyName = do
  info <- reify tyName
  case info of
    TyConI (DataD context _name tyvars cons _derivs)
      | length cons > 255 -> fail $ "Can't derive entities for: " ++ show tyName ++
                                    ". The datatype must have less than 256 constructors."
      | otherwise         -> worker context tyvars (zip [0..] cons)
    TyConI (NewtypeD context _name tyvars con _derivs) ->
      worker context tyvars [(0, con)]
    FamilyI _ insts -> do
      decs <- forM insts $ \inst ->
        case inst of
          DataInstD context _name ty cons _derivs ->
              worker' (foldl appT (conT tyName) (map return ty)) context [] (zip [0..] cons)
          NewtypeInstD context _name ty con _derivs ->
              worker' (foldl appT (conT tyName) (map return ty)) context [] [(0, con)]
          _ -> fail $ "Can't derive entities instance for: " ++ show (tyName, inst)
      return $ concat decs
    _ -> fail $ "Can't derive entities instance for: " ++ show (tyName, info)
  where
    worker = worker' (conT tyName)
    worker' tyBase context tyvars cons = sequence $ mkFields (map snd cons)
    cFields = map (\(name, _, _) -> mkName $ (nameBase name))
    prfField name = mkName $ "_" ++ (nameBase name)
    mkField name = valD (varP $ prfField name) (normalB $ conE 'Entity `appE` ((varE 'T.pack) `appE` (litE $ stringL $ nameBase name))) []
    mkFields cons = [ mkField field | (RecC _ fields) <- cons, field <- cFields fields ]
