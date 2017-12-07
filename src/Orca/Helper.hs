module Orca.Helper where
import Orca.Reader.Greyscale
import Orca.Reader.Processing
import Orca.Testing
import Graphics.Image.IO
import Graphics.Image (dims, Bit, X, VS, Image, RGB, RGBA, Readable, rotate90) 
import Graphics.Image.Processing.Filter(sobelOperator,prewittOperator)
import Graphics.Image.Processing(crop)
import Graphics.Image.Interface
import Data.Int(Int64)
import qualified Data.Vector.Storable as V
import Data.Monoid(mconcat)
import Control.Monad(liftM)

writeHistogram :: FilePath -> [Int] -> IO ()
writeHistogram fp hsdata = writeFile fp d8ta
    where d8ta = concat $ flip (++) "\n" . show <$> hsdata

prewittMain :: IO ()
prewittMain = do
    tryWithGrey testImageSource PNG $ display . prewittOperator --(\x-> putStrLn "" ) --display

threshMain :: IO ()
threshMain = do
    putStrLn "Enter a divisor"
    divisor <- readLn
    putStrLn "Enter a threshold"
    t <- readLn
    trySmallDisplay divisor t
    trySmallTest (display . adaptThresholdPart3) divisor t
    trySmallTest (display . adaptThresholdPart32) divisor t
    tryBigDisplay divisor t
    tryBigTest (display . adaptThresholdPart3 . rotate90) divisor t
    tryBigTest (display . adaptThresholdPart32 . rotate90) divisor t

tryThresholdingWith :: Readable (Image VS RGB Double) format => String -> format -> (Image VS X Bit -> IO ()) -> Int -> Double -> IO()
tryThresholdingWith filepath format f divisor t = tryWithGrey filepath format $ f . (adaptiveThresholdRatio divisor t)

tryBigTest = tryThresholdingWith testBigImageSource JPG
trySmallTest = tryThresholdingWith testImageSource PNG
trySecondTest = tryThresholdingWith testSecondImageSource JPG

display :: (Writable (Image VS cs e) TIF, Array arr cs e, V.Storable (Pixel cs e)) => Image arr cs e -> IO ()
display = displayImageUsing defaultViewer True

tryBigDisplay = tryBigTest (display . rotate90)
trySmallDisplay = trySmallTest display 
trySecondDisplay = trySecondTest display

{-showGrey :: String -> IO ()
showGrey str = tryWithGrey str PNG $ \x -> do
    displayImageUsing defaultViewer True threshed
-}