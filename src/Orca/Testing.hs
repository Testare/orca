module Orca.Testing where
import Orca.Reader.Clustering
import Codec.Picture
import Codec.Picture.Types
import System.IO

testImageSource = "./test_source4.png"
testSecondImageSource = "./test_source2.JPG"
testBigImageSource = "./test_source.JPG"

attemptDirectory = "./data/attempts/"
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
