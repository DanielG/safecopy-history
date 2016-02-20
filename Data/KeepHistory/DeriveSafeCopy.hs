module Data.KeepHistory.DeriveSafeCopy where

import Data.SafeCopy.Internal
import Data.KeepHistory.Types

import Language.Haskell.TH

deriveSafeCopyHistory :: HistoricDataK -> Q [Dec]
deriveSafeCopyHistory (HistoricDataK ver _ dk@(DataK tyn _ _)) =
    internalDeriveSafeCopy'
      Normal
      (fromInteger ver)
      (if ver == 0 then 'base else 'extension)
      tyn
      (TyConI (fromDataK dk))
