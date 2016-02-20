{-# OPTIONS -fno-warn-orphans #-}
module Data.KeepHistory.Types where

import Control.Arrow (first)
import Data.Generics

import Language.Haskell.TH.Syntax

data HistoricDataK = HistoricDataK {
      histVer :: Integer
    , histRefs :: Maybe [(Name, Integer)] -- must always be 'Just' on disk
    , histData :: DataK
    }
    deriving (Eq, Ord, Read, Show, Data)
data DataK = DataK Name [TyVarBndr] [ConK]
    deriving (Eq, Ord, Read, Show, Data)
data ConK = NormalConK Name [StrictType]
          | RecConK Name [VarStrictType]
     deriving (Eq, Ord, Read, Show, Data)

deriving instance Read Type
deriving instance Read TyLit
deriving instance Read Strict
deriving instance Read TyVarBndr

instance Read Name where
    readsPrec _ = map (first mkName) <$> lex

toDataK :: Dec -> DataK
toDataK (DataD _ctx tyn tyvars cs _deriv) =
      DataK tyn tyvars (map toConK cs)
toDataK _ = error "toHistoricData: unsupported declaration"

toConK :: Con -> ConK
toConK (NormalC tyn tys) = NormalConK tyn tys
toConK (RecC tyn tyfs)   = RecConK tyn tyfs
toConK _ = error "toHistoricCon: unsupported constructor"

fromDataK :: DataK -> Dec
fromDataK (DataK dtyn tyvars cs) =
    DataD [] dtyn tyvars (map fromConK cs) []

fromConK :: ConK -> Con
fromConK (NormalConK tyn tys) =
    NormalC tyn tys
fromConK (RecConK tyn vtys) =
    RecC tyn vtys

dataKTyn :: DataK -> Name
dataKTyn (DataK tyn _ _ ) = tyn

dataDTyn :: Dec -> Name
dataDTyn (DataD _ tyn _ _ _ ) = tyn
dataDTyn _ = error "dataDTyn: unsupported declaration"

conKName :: ConK -> Name
conKName (NormalConK cn _stys)      = cn
conKName (RecConK cn _vstys)        = cn
