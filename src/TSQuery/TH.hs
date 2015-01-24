{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module TSQuery.TH where

import           Language.Haskell.TH

import           Control.Monad

import           Data.List
import qualified Data.Text                    as T

import           TSQuery.Query

data O

mkTSEntities :: Bool -> Name -> Q [Dec]
mkTSEntities _literal tyName = do
  info <- reify tyName
  case info of
    TyConI (DataD _ _ _ cons _)
      | length cons > 255 -> fail $ "Can't derive entities for: " ++ show tyName ++
                                    ". The datatype must have less than 256 constructors."
      | otherwise         -> mk cons
    TyConI (NewtypeD _ _ _ con _) ->
      mk [con]
    FamilyI _ insts -> do
      decs <- forM insts $ \inst ->
        case inst of
          DataInstD _ _ _ cons _ ->
              mk cons
          NewtypeInstD _ _ _ con _ ->
              mk [con]
          _ -> fail $ "Can't derive entities instance for: " ++ show (tyName, inst)
      return $ concat decs
    _ -> fail $ "Can't derive entities instance for: " ++ show (tyName, info)
  where
    conFields          = map (\(name, _, ty) -> (ty, mkName $ (nameBase name)))
    fmtName name       = mkName $ "_" ++ (nameBase name)
    mkVal name         = valD (varP $ fmtName name)
                              (normalB $ conE 'Entity
                                       `appE` ((varE 'T.pack)
                                       `appE` (litE $ stringL $ nameBase name))) []

    mkSig (ConT tyN) name
                       = sigD (fmtName name)
                              (conT ''Entity `appT` conT tyName `appT` conT tyN)
    mkSig _ _          = fail "Can't derive entity declaration for complex type"

    mkField (ty, name) = [mkSig ty name, mkVal name]
    mkFields cons      = concatMap mkField $ nub
                           [ field
                           | (RecC _ fields) <- cons
                           , field <- conFields fields ]
    mk                 = sequence . mkFields

