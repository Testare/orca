module Orca.Training where
import Graphics.Image.IO(writeImage,writeImageExact, PNG(..))
import Orca.Reader.Layout(Symbol, symbolImage)
import Orca.Helper(display)
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

symbolFilename :: Char -> Int -> [Char]
symbolFilename sym i = sym:'_':(show i) ++ ".png"

getTrainingFilesForSymbol :: Char -> IO [FilePath]
getTrainingFilesForSymbol = getFilesInPathForSymbol fpTestingDataSet

getFilesInPathForSymbol :: FilePath -> Char -> IO [FilePath]
getFilesInPathForSymbol fp sym = filter ((== sym) . head) <$> listDirectory fp

addSymbolToDataSet :: FilePath -> Symbol -> Char -> IO ()
addSymbolToDataSet fp symbol sym = do
    filename <- ((++) fp) <$> symbolFilename sym <$> length <$> getFilesInPathForSymbol fp sym :: IO FilePath
    writeImageExact PNG [] filename (symbolImage symbol)

--main :: IO ()
--Ask for a filename, call the thresholding subroutine on it.
--

addSymbolsToDataSets :: Int -> [Symbol] -> IO ()
addSymbolsToDataSets _ [] = putStrLn "No more symbols!"
addSymbolsToDataSets n (symbol:symbols) = do
    display $ symbolImage symbol
    putStrLn $ (++) (show n ++ ": ") $ show symbol
    putStrLn "(0) Quit, (1) Training, (2) Testing DataSet, (3) Skip?"
    fp <- ((!!) [Left (putStrLn "Finished"), Right fpTrainingDataSet, Right fpTestingDataSet, Left (addSymbolsToDataSets (succ n) symbols)]) <$> readLn :: IO (Either (IO ()) FilePath)
    flip (either id) fp $ \x -> do
        putStrLn "What's the symbol key?"
        sym <- last <$> getLine :: IO Char
        addSymbolToDataSet x symbol sym 
        addSymbolsToDataSets (succ n) symbols












