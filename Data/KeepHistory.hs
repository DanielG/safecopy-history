{-# LANGUAGE ViewPatterns
  , TemplateHaskell
  , TupleSections
  , StandaloneDeriving
  , DeriveDataTypeable #-}
module Data.KeepHistory (keepHistory, testKeepHistory) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Generics
import Control.Arrow
import Control.Monad
import Control.DeepSeq
import Control.Exception
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.FilePath
import System.Directory
import Text.Read

import Text.Show.Pretty (ppShow)

import Data.KeepHistory.Types
import Data.KeepHistory.DeriveSafeCopy

data NSName = TypeName Name | ValueName Name
    deriving (Eq, Ord, Read, Show, Data)

keepHistory :: Q [Dec] -> Q [Dec]
keepHistory qds = do
  hs' <- readHistory
  (decs, hs) <- keepHistory' hs' qds
  writeHistory hs
  return decs

keepHistory' :: [HistoricDataK] -> Q [Dec] -> Q ([Dec], [HistoricDataK])
keepHistory' hs qds = do
  ds <- qds
  let hs' = fixupHistoryReferences $
              concatMap (\d -> extendHistory (clean $ toDataK d) $ filterHistory d hs) ds
  ds' <- forM ds $ \d -> do
    let drhs = map renameDataK $ filterHistory d hs'
        hds = map (fromDataK . histData) drhs
    dds <- concat <$> deriveSafeCopyHistory `mapM`
             if length drhs >= 2 then reverse $ drop 1 $ reverse drhs else drhs
    return $ d : (hds ++ dds)

  return $ (concat ds', hs')


testKeepHistory :: [String] -> Q [Dec] -> Q [Dec] -> [String] -> Q [Dec]
testKeepHistory hist qds qsds hist' = do
  let hs = map read hist
  (ds', hs') <- keepHistory' hs qds

  let isInstanceD (InstanceD _ _ _) = True
      isInstanceD _ = False

  let ds = filter (not . isInstanceD) ds'

  sds <- qsds

  forM_ (zipMaybe (clean sds) (clean ds)) $ \(a, b) ->
    when (not $ a == b) $ do
      runIO $ do
        putStrLn "failure "
        putStrLn ""
        putStrLn $ "should: \n" ++ ppShow (clean sds)
        putStrLn $ "is    : \n" ++ ppShow (clean ds)
        putStrLn ""
        putStrLn $ "should: " ++ ppShow a
        putStrLn $ "is    : " ++ ppShow b
      fail "test failed"

  forM_ (zipMaybe hist' (map show hs')) $ \(a, b) ->
    when (not $ a == b) $ do
      runIO $ do
        putStrLn "history failure "
        putStrLn ""
        putStrLn $ "should: \n" ++ ppShow (map read hist' :: [HistoricDataK])
        putStrLn $ "is    : \n" ++ ppShow (hs' :: [HistoricDataK])
        putStrLn ""
        putStrLn $ "should: " ++ show a
        putStrLn $ "is    : " ++ show b
        putStrLn $ "should: " ++ ppShow (fmap read a :: Maybe HistoricDataK)
        putStrLn $ "is    : " ++ ppShow (fmap read b :: Maybe HistoricDataK)
      fail "test failed"


  return []

zipMaybe :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipMaybe as bs = let
    mas = map Just as
    mbs = map Just bs
    (mas', mbs') = if length as < length bs
                     then (mas ++ repeat Nothing, mbs)
                     else (mas, mbs ++ repeat Nothing)
  in
    zip mas' mbs'


readHistory :: Q [HistoricDataK]
readHistory = do
  fn <- loc_filename <$> location
  mf <- runIO $ do
          mf <- readFileMaybe (dropExtension fn <.> "hsitory")
          evaluate $ force mf

  case mapM readMaybe =<< (lines <$> mf) of
    Just hs -> return hs
    Nothing -> return []

filterHistory :: Dec -> [HistoricDataK] -> [HistoricDataK]
filterHistory (dataDTyn -> tyn) hist =
    filter ((==cleanName tyn) . historicDataKTyn) hist

writeHistory :: [HistoricDataK] -> Q ()
writeHistory hs = do
  fn <- loc_filename <$> location
  let hfn = (dropExtension fn <.> "hsitory")
  runIO $ writeFile hfn $ unlines $ map show $ clean hs

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe f = do
  e <- doesFileExist f
  if e
     then Just <$> readFile f
     else return Nothing

extendHistory :: DataK -> [HistoricDataK] -> [HistoricDataK]
extendHistory dk hs
    | elem (clean dk) $ map histData hs = hs
    | otherwise = hs ++ [HistoricDataK nextVer Nothing dk]
 where
   nextVer = 1 + maximum ((-1) : map histVer hs)

fixupHistoryReferences :: [HistoricDataK] -> [HistoricDataK]
fixupHistoryReferences hs = map fixRefs hs
 where
   m :: Map Name Integer
   m = Map.fromListWith max $ map (historicDataKTyn &&& histVer) $ hs

   lookupRef :: Name -> Maybe (Name, Integer)
   lookupRef n = (n,) <$> Map.lookup n m

   fixRefs (HistoricDataK ver refs dk) = let
       refs' = catMaybes $ map lookupRef $ collectConTs dk
     in
       HistoricDataK ver (if refs == Nothing then Just refs' else refs) dk

renameDataK :: HistoricDataK -> HistoricDataK
renameDataK (HistoricDataK ver (Just hns') (DataK dtyn tyvars cons)) =
    HistoricDataK ver (Just hns') $ DataK
      (mkHName ver dtyn)
      (map (renameTyVar hns) tyvars)
      (map (renameConK ver hns) cons)
 where
   hns = [(TypeName dtyn, ver)]
         ++ map ((,ver) . ValueName . conKName) cons
         ++ map (first TypeName) hns'
renameDataK _ = error "renameDataK"

renameConK :: Integer -> [(NSName, Integer)] -> ConK -> ConK
renameConK _ver hns (NormalConK cn stys) =
    NormalConK (renameValueName hns cn) (map (second (renameType hns)) stys)
renameConK ver hns (RecConK cn vstys) =
    RecConK (renameValueName hns cn) (map renameRecField vstys)
 where
   renameRecField (fn,s,t) =
       (mkHName ver fn, s, renameType hns t)

renameType :: [(NSName, Integer)] -> Type -> Type
renameType hns ty = everywhere (mkT appendVer) ty
 where
   appendVer (ConT n) = ConT $ renameTypeName hns n
   appendVer x = x

renameTyVar :: [(NSName, Integer)] -> TyVarBndr -> TyVarBndr
renameTyVar _ (PlainTV tvn) =
    PlainTV tvn
renameTyVar hns (KindedTV tvn k) =
    KindedTV tvn $ renameType hns k

renameValueName :: [(NSName, Integer)] -> Name -> Name
renameValueName hns n
    | Just ver <- lookup (ValueName n) hns = mkHName ver n
    | otherwise = n

renameTypeName :: [(NSName, Integer)] -> Name -> Name
renameTypeName hns n
    | Just ver <- lookup (TypeName n) hns = mkHName ver n
    | otherwise = n

mkHName :: Integer -> Name -> Name
mkHName ver n = appendName ("_v" ++ show ver) n

appendName :: String -> Name -> Name
appendName str (Name (OccName n) _) = mkName (n ++ str)

collectConTs :: Data a => a -> [Name]
collectConTs = map unConT . listify isConT
 where
   isConT (ConT _) = True
   isConT _ = False
   unConT (ConT x) = x
   unConT _ = error "collectConTs.unConT"

clean :: Data a => a -> a
clean = everywhere (mkT cleanName)

-- TODO: handle qualified names!!!
cleanName :: Name -> Name
cleanName (Name ocn _nf) = Name ocn NameS

historicDataKTyn :: HistoricDataK -> Name
historicDataKTyn (HistoricDataK _ _ dk) = dataKTyn dk
