import Orca.Helper
import Orca.App(selectOp)
import Orca.Training(addSymbolsToDataSets3,addSymbolsToDataSets)
import Orca.Reader.Layout
import Orca.Reader.Processing

import Orca.Testing
import Graphics.Image (dims, JPG(..), Bit, X, VS, Image, RGB (RedRGB, GreenRGB,BlueRGB), RGBA, Readable, rotate90)
import Graphics.Image.Interface
import Data.List(sortOn, intercalate)

{-
    Pretty ramshackle code here...
    Most of it copied from Research.hs
    In the future, most of these functions should be moved to
    a generic helper module for the "main" scripts

    
-}

main :: IO ()
main = selectOp $
      [ ("Old Training", testTraining False)
      , ("Quick training", testTraining2 False)
      ]

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
        addSymbolsToDataSets k $ drop k $ symbolList
        ) divisor t

testTraining2 :: Bool -> IO ()
testTraining2 doFiltering1 = do
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

filterFunc :: Symbol -> Bool
filterFunc = do
    long <- filterFunc_long
    small <- filterFunc_small
    return (long && small)

filterFunc_long :: Symbol -> Bool
filterFunc_long = (\x-> maximum x < 5*(minimum x) ). (<*>) [fst,snd] . pure . symbolDims

filterFunc_small :: Symbol -> Bool
filterFunc_small = ((<) 10) . symbolWeight