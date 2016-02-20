{-# LANGUAGE TemplateHaskell #-}

import Data.KeepHistory
import Data.SafeCopy ()

-- Works at all
testKeepHistory [] [d|
 data D = D E
 data E = E D
 |]
 [d|
    data D = D E
    data D_v0 = D_v0 E_v0
    data E = E D
    data E_v0 = E_v0 D_v0
 |]
 [
   "HistoricDataK {histVer = 0, histRefs = Just [(E,0)], histData = DataK D [] [NormalConK D [(NotStrict,ConT E)]]}"
 , "HistoricDataK {histVer = 0, histRefs = Just [(D,0)], histData = DataK E [] [NormalConK E [(NotStrict,ConT D)]]}"
 ]

-- Doesn't change output when history present but nothing changed
testKeepHistory [
 "HistoricDataK {histVer = 0, histRefs = Just [(E,0)], histData = DataK D [] [NormalConK D [(NotStrict,ConT E)]]}",
 "HistoricDataK {histVer = 0, histRefs = Just [(D,0)], histData = DataK E [] [NormalConK E [(NotStrict,ConT D)]]}"
 ]
 [d|
 data D = D E
 data E = E D
 |]
 [d|
    data D = D E
    data D_v0 = D_v0 E_v0
    data E = E D
    data E_v0 = E_v0 D_v0
 |]
 [
   "HistoricDataK {histVer = 0, histRefs = Just [(E,0)], histData = DataK D [] [NormalConK D [(NotStrict,ConT E)]]}"
 , "HistoricDataK {histVer = 0, histRefs = Just [(D,0)], histData = DataK E [] [NormalConK E [(NotStrict,ConT D)]]}"
 ]


-- Handles single updates
testKeepHistory [
 "HistoricDataK {histVer = 0, histRefs = Just [(E,0)], histData = DataK D [] [NormalConK D [(NotStrict,ConT E)]]}",
 "HistoricDataK {histVer = 0, histRefs = Just [(D,0)], histData = DataK E [] [NormalConK E [(NotStrict,ConT D)]]}"
 ]
 [d|
 data D = D E
 data E = E D | F
 |]
 [d|
    data D = D E
    data D_v0 = D_v0 E_v0
    data E = E D | F
    data E_v0 = E_v0 D_v0
    data E_v1 = E_v1 D_v0 | F_v1
 |]
 [
   "HistoricDataK {histVer = 0, histRefs = Just [(E,0)], histData = DataK D [] [NormalConK D [(NotStrict,ConT E)]]}"
 , "HistoricDataK {histVer = 0, histRefs = Just [(D,0)], histData = DataK E [] [NormalConK E [(NotStrict,ConT D)]]}"
 , "HistoricDataK {histVer = 1, histRefs = Just [(D,0)], histData = DataK E [] [NormalConK E [(NotStrict,ConT D)],NormalConK F []]}"
 ]


-- Handles multiple simultanious updates
testKeepHistory [
 "HistoricDataK {histVer = 0, histRefs = Just [(E,0)], histData = DataK D [] [NormalConK D [(NotStrict,ConT E)]]}",
 "HistoricDataK {histVer = 0, histRefs = Just [(D,0)], histData = DataK E [] [NormalConK E [(NotStrict,ConT D)]]}"
 ]
 [d|
     data D = D E | G
     data E = E D | F
 |]
 [d|
     data D = D E | G
     data D_v0 = D_v0 E_v0
     data D_v1 = D_v1 E_v1 | G_v1
     data E = E D | F
     data E_v0 = E_v0 D_v0
     data E_v1 = E_v1 D_v1 | F_v1
 |]
 [ "HistoricDataK {histVer = 0, histRefs = Just [(E,0)], histData = DataK D [] [NormalConK D [(NotStrict,ConT E)]]}"
 , "HistoricDataK {histVer = 1, histRefs = Just [(E,1)], histData = DataK D [] [NormalConK D [(NotStrict,ConT E)],NormalConK G []]}"
 , "HistoricDataK {histVer = 0, histRefs = Just [(D,0)], histData = DataK E [] [NormalConK E [(NotStrict,ConT D)]]}"
 , "HistoricDataK {histVer = 1, histRefs = Just [(D,1)], histData = DataK E [] [NormalConK E [(NotStrict,ConT D)],NormalConK F []]}"
 ]

main :: IO ()
main = return ()
