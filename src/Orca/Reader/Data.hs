module Orca.Reader.Data ( readEigenDataFolder
                        , readAlphaDataFolder
                        , readZetaDataFolder
                        , combineDatasets
                        , onlyAccepting
                        , ignoreString_accept26
                        , acceptString_accuracy96
                        , acceptString_accuracy81
                        , ignoreString_accuracy96
                        , ignoreString_accuracy81) where

import Orca.Reader.Types

import qualified Graphics.Image.IO as HipIO
import Graphics.Image(Image, VS, X, Bit, PNG)
import Graphics.Image.ColorSpace
import System.Directory(listDirectory)
import System.FilePath(FilePath, (</>))

import qualified Data.Map as M

acceptString_fullaccept = "$()+,-.0123456789;=@ABCDEFGHIJKLMNOPQRSTUVWXYZ[^_abcdefghijklmnopqrstuvwxyz"
ignoreString_accuracy96 = "$)+,-.04678;@BCDFGKLMNOQRSTUVXYZ[acdefghiklnqstuxy"
acceptString_accuracy96 = "(12359=AEHIJPW^_bjmoprvwz"
ignoreString_accuracy81 = "$,-.0478;BCDFGKLNOQSTUVXZ[cdefhiklnqsu"
acceptString_accuracy81 = "()+123569=@AEHIJMPRWY^_abgjmoprtvwxyz"
ignoreStringDiff = ")+6@MRYagtxy" 
ignoreString_accept26 = "$),-.04678;BCDFGHKLMNOQRSTUVXYZ[acdefghiklnqstuxy" --100% accuracy baby!
acceptString_accept26 = "(+12359=@AEIJPW^_bjmoprvwz"

onlyAccepting :: M.Map SymbolName a -> String -> M.Map SymbolName a
onlyAccepting dataset acceptStr = foldl (flip M.delete) dataset $ map (tail . stringToSymbolName . pure) ignoreStr
    where ignoreStr = filter (`notElem` acceptStr) acceptString_fullaccept

{- Is a dataset a monoid candidate? -}
combineDatasets :: M.Map String [Image VS X Bit] -> M.Map String [BitImage] -> M.Map String [BitImage]
combineDatasets = M.unionWith mappend

readData :: DatasetType -> FilePath -> IO TDataset
readData Alpha = readAlphaData
readData Eigen = readEigenData
readData Zeta = readZetaData
--readData Eigen = error "Not currently implemented"

{-Private-}
readAlphaData :: FilePath -> IO TDataset
readAlphaData = fmap (\a -> mempty { alpha = a} ) . readAlphaDataFolder 

readEigenData :: FilePath -> IO TDataset
readEigenData = fmap (\e -> mempty {eigen = e}) . readEigenDataFolder

readZetaData :: FilePath -> IO TDataset 
readZetaData = fmap (\z -> mempty {zeta = z}) . readZetaDataFolder

readAlphaDataFolder :: FilePath -> IO AlphaData
readAlphaDataFolder = readDatasetFolder

readEigenDataFolder :: FilePath -> IO EigenData
readEigenDataFolder = readDatasetFolder

readZetaDataFolder :: FilePath -> IO ZetaData
readZetaDataFolder = fmap M.fromList . readDatasetFolder'

readDatasetFolder :: HipIO.Readable img PNG => FilePath -> IO (M.Map String [img])
readDatasetFolder = ((M.fromListWith mappend) <$>) . ((map (pure <$>)) <$>) . readDatasetFolder'

readDatasetFolder' :: HipIO.Readable img PNG => FilePath -> IO [(String, img)]
readDatasetFolder' fp = do
    imageFiles <- listDirectory fp
    let imageFilepaths = (fp </>) <$> imageFiles
    imageData <- sequence $ HipIO.readImageExact HipIO.PNG <$> imageFilepaths
    return $ foldl f []$ zip (map filenameToSymbolName imageFiles) imageData
    where f acc (_, (Left _)) = acc
          f acc (str, (Right img)) = (str,img):acc

filenameToSymbolName :: String -> SymbolName
filenameToSymbolName = take symbolNameLength . tail

--readDatasetFolder' :: String -> [(Image VS X Bit, String)]
--readDatasetFolder' fp = ((hipIO.readImageExact PNG . (fp ++ )) <$>) <$> (\x -> (x, take 5 x)) <$> listDirectory fp

--readDatasetFolder :: String -> Map String [Image VS X Bit]
--readDatasetFolder fp = <$> (\x -> (x, take 5 x)<$> listDirectory fp