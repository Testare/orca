import Orca.Reader.Greyscale
import Orca.Reader.Processing
import Orca.Reader.Layout
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

main :: IO ()
main = do
    putStrLn "/* Choose op */"
    putStrLn $ displayOps ops
    integer <- readLn
    snd $ ops !! integer

writeHistogram :: FilePath -> [Int] -> IO ()
writeHistogram fp hsdata = writeFile fp d8ta
    where d8ta = concat $ flip (++) "\n" . show <$> hsdata

ops :: [(String, IO ())]
ops = [ ("Grey thresholding", threshMain)
      , ("Prewitt operator", prewittMain) 
      , ("Test battery", runFullTests)
      ]

displayOps :: [(String, IO ())] -> String
displayOps = disp 0 
    where disp n [] = ""
          disp n (x:xs) = (show n) ++ ". " ++ (fst x) ++ "\n" ++ (disp (succ n) xs)

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

{-
main :: IO ()
main = do
    let imgs = [testImageSource, testBigImageSource]
    putStrLn $ "Use big image? (0=small,1=big)" imgFile <- (readLn) >>= (pure . (imgs !!))
    putStrLn $ "Write stats?(y/n)"
    writeStatsBool <- getLine >>= return . (== "y")
    putStrLn $ "Would you like to apply a threshold? (y/n)"
    applyThreshIn <- (getLine :: IO [Char])
    imgModFn <- if applyThreshIn /= "y" then return id else do 
        putStrLn $ "Lower bound:"
        low <- readLn
        putStrLn $ "High bound:"
        high <- readLn
        putStrLn $ "CutOff or BinaryImage?"
        threshOp <- readLn
        return $ threshold threshOp low high

    tryWithImage imgFile $ \img -> do

        let greyImg = imgModFn $ convertToGreyscale img
        let hsdata = histogramToList $ createHistogram $ greyImg
        counter <- getCounter greyAttemptCounter >>= (return . show)
        incCounter greyAttemptCounter
        let outimg = (greyAttemptDirectory ++ counter ++ targetFile)
        putStrLn $ "Putting this where it belongs: " ++ outimg
        if writeStatsBool then do
            writePng outimg $ greyImg
            let outHs = (greyAttemptDirectory ++ counter ++ histogramFile)
            putStrLn $ "Writing histogram data: " ++ outHs
            writeHistogram outHs hsdata
            else return ()
-}