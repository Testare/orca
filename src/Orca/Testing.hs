module Orca.Testing where

import Codec.Picture
import Codec.Picture.Types

import Graphics.Image(PNG, JPG)

import System.IO
import Control.Monad(liftM)
import System.Directory(listDirectory)
import Data.List(isSuffixOf)

testImageSource = "./test_source4.png"
testSecondImageSource = "./test_source2.JPG"
testBigImageSource = "./test_source.JPG"
testAlphabetSource = "./orcan.jpg"

attemptDirectory = "./data/attempts/"
testBatteryDirectory = "./data/tests/"
testBatteryInputDirectory = "./data/testImgs/"

greyAttemptDirectory = attemptDirectory ++ "grey/"
greyAttemptCounter = greyAttemptDirectory ++ "counter.txt"
counterFile = "counter.txt"

targetFile = "_target.png"
histogramFile = "_stats.txt"

testImageTarget = "./test_target.png"

testImageTarget2 = "./test_target2.png"
testImageTarget3 = "./test_target3.png"
testBlackImageTarget = "./black_target.png"
testWhiteImageTarget = "./white_target.png"
testRedImageTarget = "./red_target.png"
testGreenImageTarget = "./green_target.png"
testBlueImageTarget = "./blue_target.png"
testGrayImageTarget = "./test_target_gray.png"

getCounter :: FilePath -> IO Int
{-getCounter counterFile = do
    counterHandle <- openFile counterFile ReadMode
    contents <- hGetContents counterHandle
    return $ read contents
    -}
getCounter counterFile = withFile counterFile ReadMode (\handle -> (hGetLine handle) >>= (return . read))

setCounter :: FilePath -> Int -> IO ()
setCounter counterFile = writeFile counterFile . show

incCounter :: FilePath -> IO ()
incCounter counterFile = (getCounter counterFile) >>= (setCounter counterFile . succ)

{-- TEST BATTERY CODE--}

data TestImage = TestImagePng FilePath | TestImageJpg FilePath -- Filename, format
type TestCase = String -- Stub
data Op = Op String (TestImage -> TestCase -> IO String)

filepath :: TestImage -> FilePath
filepath (TestImagePng fp) = fp
filepath (TestImageJpg fp) = fp

testImages :: IO [TestImage]
testImages = do
    liftM (map f) testPaths
    where testPaths = listDirectory testBatteryInputDirectory
          f x
            | isSuffixOf "jpg" x || isSuffixOf "JPG" x = TestImageJpg x
            | isSuffixOf "png" x || isSuffixOf "PNG" x = TestImagePng x
            | otherwise = TestImagePng x

testOps :: [Op]
testOps = [ Op "name" (\img tstcase-> return $ filepath img)
          , Op "charcnt" (\img tstcase-> return $ show $ length $ filepath img)]

testCases :: [TestCase]
testCases = ["no-params", "extra-no-params"]

testCaseName :: TestCase -> String
testCaseName = id

performOp :: Op -> TestImage -> TestCase -> IO String
performOp (Op _ x) = x

opName :: Op -> String
opName (Op nm _) = nm

runTest :: TestImage -> TestCase -> [Op] -> IO String
runTest img tst ops = do
    let header = liftM (\x -> (filepath img) ++ "-" ++ (testCaseName tst) ++ "\n" ++ x ++ "\n")
    let opResults = sequence $ map (\x -> liftM (\y -> opName x ++ ": " ++ y ++ "\n") $ (performOp x) img tst) ops :: IO [String]
    header $ liftM concat opResults
    
runTests :: [TestImage] -> [TestCase] -> [Op] -> [IO String]
runTests imgs tsts ops = (map (flip ($) ops) allTests)
    where allTests = runTest <$> imgs <*> tsts :: [[Op] -> IO String]

runTestsFrom :: [TestImage] -> [TestCase] -> [Op] -> Int -> IO String 
runTestsFrom imgs tsts ops = (!!) (runTests imgs tsts ops)

runFullTests :: IO ()
runFullTests = do
    testImgs <- testImages
    let tests = runTests testImgs testCases testOps
    (concat <$> sequence tests) >>= putStrLn
