module Orca.Reader.Data (readDatasetFolder) where

import Orca.Reader.Types

import qualified Graphics.Image.IO as HipIO
import Graphics.Image(Image, VS, X, Bit, PNG)
import Graphics.Image.ColorSpace
import System.Directory(listDirectory)
import System.FilePath((</>))

import qualified Data.Map as M

{- Is a dataset a monoid candidate? -}
combineDatasets :: M.Map String [Image VS X Bit] -> M.Map String [BitImage] -> M.Map String [BitImage]
combineDatasets = M.unionWith mappend

readDatasetFolder :: String -> IO (M.Map String [Image VS X Bit])
readDatasetFolder = ((M.fromListWith mappend) <$>) . ((map (pure <$>)) <$>) . readDatasetFolder'

readDatasetFolder' :: HipIO.Readable img PNG => String -> IO [(String, img)]
readDatasetFolder' fp = do
    imageFiles <- listDirectory fp
    let imageFilepaths = (fp </>) <$> imageFiles
    imageData <- sequence $ HipIO.readImageExact HipIO.PNG <$> imageFilepaths
    return $ foldl f []$ zip (map filenameToSymbolName imageFiles) imageData
    where f acc (_, (Left _)) = acc
          f acc (str, (Right img)) = (str,img):acc

filenameToSymbolName :: String -> String
filenameToSymbolName = take symbolNameLength . tail

--readDatasetFolder' :: String -> [(Image VS X Bit, String)]
--readDatasetFolder' fp = ((hipIO.readImageExact PNG . (fp ++ )) <$>) <$> (\x -> (x, take 5 x)) <$> listDirectory fp

--readDatasetFolder :: String -> Map String [Image VS X Bit]
--readDatasetFolder fp = <$> (\x -> (x, take 5 x)<$> listDirectory fp