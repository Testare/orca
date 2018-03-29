module Orca.Training where
import Graphics.Image.IO(writeImage,writeImageExact, PNG(..))
import Orca.Reader.Types(Symbol, SymbolName, symbolImage, symbolNameLength)
import Orca.Helper(display)
import Control.Concurrent(threadDelay)

import Data.Char(isUpper,toUpper)
import Data.List(intersperse)
import System.Directory(listDirectory)

{-
Enter filename:
-}

fpTrainingDataSet :: FilePath
fpTrainingDataSet = "./data/training/"
fpTestingDataSet :: FilePath
fpTestingDataSet = "./data/test_symbols/"
fpEigenfaces :: FilePath
fpEigenfaces = "./data/eigen/"


stringToSymbol :: [Char] -> SymbolName
stringToSymbol str = take symbolNameLength $ fstChr:str ++ (repeat '_')
    where fstChr = if isUpper $ head str then '^' else '_'

symbolFilename :: SymbolName -> Int -> [Char]
symbolFilename sym i = sym ++ '_':(show i) ++ ".png"

getTrainingFilesForSymbol :: SymbolName -> IO [FilePath]
getTrainingFilesForSymbol = getFilesInPathForSymbol fpTestingDataSet

getFilesInPathForSymbol :: FilePath -> SymbolName -> IO [FilePath]
getFilesInPathForSymbol fp sym = filter ((== sym) . (take symbolNameLength)) <$> listDirectory fp

addSymbolToDataSet :: FilePath -> Symbol -> SymbolName -> IO ()
addSymbolToDataSet fp symbol sym = do
    filename <- ((++) fp) <$> symbolFilename sym <$> length <$> getFilesInPathForSymbol fp sym :: IO FilePath
    writeImageExact PNG [] filename (symbolImage symbol)

--main :: IO ()
--Ask for a filename, call the thresholding subroutine on it.

addSymbolsToDataSets3 :: Int -> [Symbol] -> IO ()
addSymbolsToDataSets3 = addSymbolsToDataSets2 fpTrainingDataSet

addSymbolsToDataSets2 :: FilePath -> Int -> [Symbol] -> IO ()
addSymbolsToDataSets2 fp _ [] = putStrLn "No more symbols!"
addSymbolsToDataSets2 fp n symbols = do
    sequence $ intersperse (threadDelay 1000000) $ map dispSymbol subSymbolNums
    putStrLn "Enter symbol labels: \n[Type QUIT to end, type REDO at the end to do this one again, SKIP to skip one symbol]"
    inputWords <- words <$> getLine
    snd $ head $ filter fst 
        [ ((any ((==) "QUIT" . map toUpper) inputWords), putStrLn "Finished.")
        , ((any ((==) "REDO" . map toUpper) inputWords), addSymbolsToDataSets2 fp n symbols)
        , (True, do
            sequence $ map f $ zip subSymbols inputWords
            addSymbolsToDataSets2 fp (n + nextInc) nextSymbols
        )]
    where nextInc = 3
          (subSymbols, nextSymbols) = splitAt nextInc symbols
          subSymbolNums = zip [n..] subSymbols
          dispSymbol (i,symbol) = do 
              display $ symbolImage symbol
              putStrLn $ (++) (show (i) ++ ": ") $ show symbol
          f (symbol,word) = if (map toUpper word) == "SKIP" then return () else addSymbolToDataSet fp symbol (stringToSymbol word)


addSymbolsToDataSets :: Int -> [Symbol] -> IO ()
addSymbolsToDataSets _ [] = putStrLn "No more symbols!"
addSymbolsToDataSets n (symbol:symbols) = do
    display $ symbolImage symbol
    putStrLn $ (++) (show n ++ ": ") $ show symbol
    putStrLn "(0) Quit, (1) Training, (2) Testing DataSet, (3) Skip?"
    fp <- ((!!) [Left (putStrLn "Finished"), Right fpTrainingDataSet, Right fpTestingDataSet, Left (addSymbolsToDataSets (succ n) symbols)]) <$> readLn :: IO (Either (IO ()) FilePath)
    flip (either id) fp $ \x -> do
        putStrLn "What's the symbol key?"
        sym <- getLine :: IO SymbolName
        addSymbolToDataSet x symbol sym 
        addSymbolsToDataSets (succ n) symbols
