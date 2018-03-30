import Data.Int(Int64)
import Data.List(sortOn, intercalate)
import Data.Ratio
import Data.Monoid(mconcat)
import qualified Data.Map.Strict as M
import Control.Monad(liftM)

import Orca.App(selectOp)
import Orca.Reader.Greyscale
import Orca.Reader.Processing
import Orca.Reader.Layout
--import Orca.Reader.Types
import Orca.Training(addSymbolsToDataSets3)
import Orca.Helper
import Orca.Testing
import Orca.Reader.Data

import Graphics.Image.IO
import Graphics.Image (dims, Bit, X, VS, Image, RGB (RedRGB, GreenRGB,BlueRGB), RGBA, Readable, rotate90)
import Graphics.Image.Processing.Filter(sobelOperator,prewittOperator)
import Graphics.Image.Processing(crop)
--import Graphics.Image.Interface
import qualified Data.Vector.Storable as V



main :: IO ()
main = selectOp $ 
      [ ("Check Data Test", checkDataMain)
      {-, ("Grey thresholding", threshMain)
      , ("Prewitt operator", prewittMain) 
      , ("Test battery", runFullTests)
      , ("Test splitting", smallSplitTest)
      , ("Test big splitting", splitTest False)
      , ("Test splitting coverage", splitTestCoverage True)
      , ("Test filtered splitting", splitTest True)
      , ("Symbol data report", symbolReport)
      , ("Symbol histogram", symbolReport)
      , ("Test training", testTraining False)-}
      ]

checkDataMain :: IO ()
checkDataMain = do
    k <- readDatasetFolder "data/alpha_datasets/orcaset1.5" 
    sequence $ putStrLn <$> (\(a,b) -> '-':a ++ ':':(show $ length b)) <$> (M.toList k)
    sequence $ (map display) $ snd $ head $ (M.toList k)
    return ()

symbolHistogram :: IO ()
symbolHistogram = do
    let doFiltering = True
    putStrLn "Enter a divisor"
    divisor <- readLn
    putStrLn "Enter a threshold"
    t <- readLn
    tryBigTest (\img -> do
        let symbols = reverse $ sortOn symbolWeight $ (if doFiltering then filter (filterFunc) else id) $ imageToSymbols img
        sequence $ Prelude.map (putStrLn . show . symbolWeight) symbols
        return ()
        ) divisor t
splitTestCoverage :: Bool -> IO ()
splitTestCoverage doFiltering1 = do
    putStrLn "(0) Alphabet, (1) Test Image"
    imageIndex <- readLn
    putStrLn "Show filter? (True/False) "
    doFiltering <- readLn
    putStrLn "Enter a divisor"
    divisor <- readLn
    putStrLn "Enter a threshold"
    t <- readLn
    tryThresholdingWith ([testAlphabetSource,testBigImageSource] !! imageIndex) JPG (\img -> do
        let modifiedImg = ([id, id {-rotate90-}] !! imageIndex) img
        --display modifiedImg
        let symbolList = reverse $ sortOn symbolWeight $  imageToSymbols modifiedImg
        let filteredList = reverse $ sortOn symbolWeight $ filter (filterFunc) $ imageToSymbols modifiedImg
        let drawingList = (filteredList, 1.0 :: Double, BlueRGB) : (if doFiltering then [(symbolList, 1.0 :: Double, GreenRGB)] else [])
        putStrLn $ "Total Symbols: " ++ (show $ length $ filteredList)
        display $ overlaySymbolCoverages modifiedImg 1.0 RedRGB drawingList
        putStrLn $ "Finished."

        ) divisor t

symbolReport :: IO ()
symbolReport = do
    putStrLn "Filtering? (True, False)"
    doFiltering <- readLn
    putStrLn "Enter a divisor"
    divisor <- readLn
    putStrLn "Enter a threshold"
    t <- readLn
    tryBigTest (\img -> do
        let symbols = reverse $ sortOn symbolWeight $ (if doFiltering then filter (filterFunc) else id) $ imageToSymbols img
        let symbolDataFns = [ show . fst . symbolDims
                , show . snd . symbolDims
                , show . fst . symbolOffset
                , show . snd . symbolOffset
                , show . symbolWeight
                , show . (\x -> (fromIntegral $ numerator x)/(fromIntegral $ denominator x)) . symbolDensity
                , show . not . filterFunc_long
                , show . not . filterFunc_small
                ]
        let symbolDataResults = Prelude.map (\x-> intercalate ", " $ symbolDataFns <*> pure x) symbols

        putStrLn "Height, Width, X, Y, Weight, Density, Too Long, Too Small"
        sequence $ Prelude.map putStrLn symbolDataResults
        return ()
        ) divisor t


smallSplitTest :: IO ()
smallSplitTest = do
    putStrLn "Enter a divisor"
    divisor <- readLn
    putStrLn "Enter a threshold"
    t <- readLn
    let doFiltering = False
    trySmallTest (\img -> do
        display img
        splitRecurse $ reverse $ sortOn symbolWeight $ (if doFiltering then filter (filterFunc) else id) $ imageToSymbols img
        ) divisor t
    where splitRecurse imgs = do
            putStrLn "Show (n) next symbols: "
            nextImageCount <- readLn
            if nextImageCount == 0 then return () else do
                sequence $ Prelude.map (display . symbolImage) (take nextImageCount imgs)
                splitRecurse (drop nextImageCount imgs)

testTraining :: Bool -> IO ()
testTraining doFiltering1 = do
    putStrLn "(0) Alphabet, (1) Test Image"
    imageIndex <- readLn
    putStrLn "Filter? (True/False) "
    doFiltering <- readLn
    putStrLn "Enter a divisor"
    divisor <- readLn
    putStrLn "Enter a threshold"
    t <- readLn
    tryThresholdingWith ([testAlphabetSource,testBigImageSource] !! imageIndex) JPG (\img -> do
        let modifiedImg = ([id, rotate90] !! imageIndex) img
        display modifiedImg
        let symbolList = reverse $ sortOn symbolWeight $ (if doFiltering then filter (filterFunc) else id) $ imageToSymbols modifiedImg
        putStrLn $ "Total Symbols: " ++ (show $ length $ symbolList )
        putStrLn "Skip n symbols? "
        k <- readLn
        addSymbolsToDataSets3 k $ drop k $ symbolList
        ) divisor t

splitTest :: Bool -> IO ()
splitTest doFiltering = do
    putStrLn "Enter a divisor"
    divisor <- readLn
    putStrLn "Enter a threshold"
    t <- readLn
    tryBigTest (\img -> do
        display img
        splitRecurse $ reverse $ sortOn symbolWeight $ (if doFiltering then filter (filterFunc) else id) $ imageToSymbols img
        ) divisor t
    where splitRecurse imgs = do
            putStrLn "Show (n) next symbols: "
            nextImageCount <- readLn
            if nextImageCount == 0 then return () else do
                sequence $ Prelude.map ( display . symbolImage) (take nextImageCount imgs)
                splitRecurse (drop nextImageCount imgs)

filterFunc :: Symbol -> Bool
filterFunc = do
    long <- filterFunc_long
    small <- filterFunc_small
    return (long && small)

filterFunc_long :: Symbol -> Bool
filterFunc_long = (\x-> maximum x < 5*(minimum x) ). (<*>) [fst,snd] . pure . symbolDims

filterFunc_small :: Symbol -> Bool
filterFunc_small = ((<) 10) . symbolWeight
