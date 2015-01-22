module TSQuery.TH where

import Language.Haskell.TH

import Control.Applicative
import Control.Monad

import Data.Maybe (fromMaybe, listToMaybe)

import TSQuery.Query

internalDeriveSafeData :: Name -> Q [Dec]
internalDeriveSafeData tyName = do
  info <- reify tyName
  case info of
    TyConI (DataD context _name tyvars cons _derivs)
      | length cons > 255 -> fail $ "Can't derive SafeData instance for: " ++ show tyName ++
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
          _ -> fail $ "Can't derive SafeData instance for: " ++ show (tyName, inst)
      return $ concat decs
    _ -> fail $ "Can't derive SafeData instance for: " ++ show (tyName, info)
  where
    worker = worker' (conT tyName)
    worker' = undefined
    {-
    worker' tyBase context tyvars cons =
      let ty = foldl appT tyBase [ varT var | PlainTV var <- tyvars ]
      in (:[]) <$> instanceD (cxt $ [classP ''SafeData [varT var] | PlainTV var <- tyvars] ++ map return context)
                                       (conT ''SafeData `appT` ty)
                                       [ mkPutCopy (show tyName) (fromIntegral $ unVersion versionId) deriveType cons
                                       , mkGetCopy deriveType (show tyName) cons
                                       , valD (varP 'version) (normalB $ litE $ integerL $ fromIntegral $ unVersion versionId) []
                                       , valD (varP 'kind) (normalB (varE kindName)) []
                                       , funD 'errorTypeName [clause [wildP] (normalB $ litE $ StringL (show tyName)) []]
                                       ]
    -}
