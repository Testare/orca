module Orca.Reader.Classification 
    ( ClassificationMethod(..)
    , classifySymbol
    ) where

import Orca.Reader.Types

import Graphics.Image(Array, Image, VS, Pixel, toLists, resize, Bilinear(Bilinear), Border(Edge))
import Graphics.Image.Interface(toWord8, toListPx)
import qualified Data.Vector.Storable as V

import Data.List(minimumBy)
import qualified Data.Map as M
import Control.Applicative(ZipList(..), getZipList)

{- * Data stuff, should probably be hoisted to its own module -}

classifySymbol :: (Array VS cs e) => Params -> [Dataset cs e] -> Symbol -> String
classifySymbol params = classifySymbol' (paramClassificationMethod params) (paramImageComparisonDims params)

classifySymbol' :: (Array VS cs e) => ClassificationMethod -> (Int, Int) -> [Dataset cs e] -> Symbol -> String
classifySymbol' PCAClassifiction icDims datasets symbol = "Stub"
classifySymbol' SimpleClassification icDims datasets symbol = findMinDistanceValue distanceData
    where goodData = concat $ map (M.toList . snd) $ filter ((==) Alpha . fst) datasets
          distanceData = map (\(nam, img) -> (nam, simpleCompareImages (symbolImage symbol) img)) goodData

findMinDistanceValue :: (Ord a) => [(String, a)] -> String
findMinDistanceValue = fst . minimumBy (\x y -> compare (snd x) (snd y))

correctDimensions :: (Array VS cs e) => (Int, Int) -> Image VS cs e -> Image VS cs e
correctDimensions = resize Bilinear Edge

simpleCompareImages :: (Array VS cs e, Array VS cs' e') => Image VS cs e -> Image VS cs' e' -> Int
simpleCompareImages img1 img2 = sum $ getZipList $ compPixel <$> (imgToZipList img1) <*> (imgToZipList img2)
    where compPixel px1 px2 = abs ((px1) - (px2))

-- | Helper function for simpleCompareImages 
imgToZipList :: (Array VS cs'' e'') => Image VS cs'' e'' -> ZipList Int       
imgToZipList = ZipList . map (fromIntegral . toWord8) . concat . (map toListPx) . concat . toLists 
