{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module TSQuery.TH where

import           Language.Haskell.TH

import           Control.Monad

import           Data.List
import qualified Data.Text                    as T

import           TSQuery.Query

data O

mkTSEntities :: Name -> Q [Dec]
mkTSEntities tyName = do
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
    conFields           = map (\(fname, _, ty) -> (ty, mkName $ (nameBase fname)))
    fmtName fname       = mkName $ "_" ++ (nameBase fname)
    mkVal fname         = valD (varP $ fmtName fname)
                               (normalB $ conE 'Entity
                                        `appE` ((varE 'T.pack)
                                        `appE` (litE $ stringL $ nameBase fname))) []

    mkSig ty fname      = sigD (fmtName fname)
                               (conT ''Entity `appT` conT tyName `appT` pure ty)

    mkNameable          = instanceD (return []) (appT (conT ''Nameable) (conT tyName))
                            [funD 'name
                              [clause [varP $ mkName "_coll"]
                                      (normalB $ (varE 'T.pack) `appE`
                                                 (litE $ stringL $ show tyName))
                                      []]]

    mkField (ty, fname) = [mkSig ty fname, mkVal fname]
    mkFields cons       = mkNameable : concatMap mkField
                            (nub [ field
                                 | (RecC _ fields) <- cons
                                 , field <- conFields fields ])
    mk                  = sequence . mkFields
