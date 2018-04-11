module Orca.Reader where 

import Orca.Reader.Types
import Orca.Reader.Greyscale(adaptiveThresholdParams, colorToGrey)
import Orca.Reader.Layout(imageToSymbols, orderSymbolsToStrings)
import Orca.Reader.Classification(classifySymbols)
import Orca.Reader.Data
import Orca.Reader.PCA

import qualified Data.Map as M
import Graphics.Image.IO(readImage)
import Data.List(intersperse)

orcaReadImage :: Params -> EData -> ColorImage -> String
orcaReadImage params edata img = concat $ intersperse "\n" $ map (classifySymbols params edata) syms
    where bi = adaptiveThresholdParams params $ colorToGrey img 
          syms = orderSymbolsToStrings params $ imageToSymbols bi

orcaReadImageIO :: Params -> FilePath -> IO (Maybe String)
orcaReadImageIO params@Params{paramImageComparisonDims = d, paramAlphaTrainingData = trainingDataPaths} fp = do
    trainingData <- fmap (foldl1 (M.unionWith mappend)) $ sequence $ (map readAlphaDataFolder trainingDataPaths :: [IO AlphaData])
    let eData = alphaDataToEigenData d trainingData
    img <- readImage fp
    return $ orcaReadImage params eData <$> either (const Nothing) (Just) img 

--orcaReadImageFromFile :: Params -> Filepath -> IO String