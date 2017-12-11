import Orca.Reader.Greyscale
import Orca.Reader.Processing
import Orca.Reader.Layout
import Orca.Helper
import Orca.Testing
import Graphics.Image.IO
import Graphics.Image (dims, Bit, X, VS, Image, RGB, RGBA, Readable, rotate90)
import Graphics.Image.Processing.Filter(sobelOperator,prewittOperator)
import Graphics.Image.Processing(crop)
import Graphics.Image.Interface
import Data.Int(Int64)
import Data.List(sortOn, intercalate)
import Data.Ratio
import qualified Data.Vector.Storable as V
import Data.Monoid(mconcat)
import Control.Monad(liftM)

main :: IO ()
main = do
    putStrLn "/* Choose op */"
    putStrLn $ displayOps ops
    integer <- readLn
    snd $ ops !! integer


ops :: [(String, IO ())]
ops = [ ("Grey thresholding", threshMain)
      , ("Prewitt operator", prewittMain) 
      , ("Test battery", runFullTests)
      , ("Test splitting", smallSplitTest)
      , ("Test big splitting", splitTest False)
      , ("Test second splitting", testSplitting2)
      , ("Test filtered splitting", splitTest True)
      , ("Symbol data report", symbolReport)
      , ("Symbol histogram", symbolReport)
      ]

testFilteredSplitting :: IO ()
testFilteredSplitting = putStrLn "Coming soon!"

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
    trySmallTest (\img -> do
        display img
        testSplitting img
        splitRecurse (getSymbolImages img)
        ) divisor t
    where splitRecurse imgs = do
            putStrLn "Show (n) next symbols: "
            nextImageCount <- readLn
            if nextImageCount == 0 then return () else do
                sequence $ Prelude.map display (take nextImageCount imgs)
                splitRecurse (drop nextImageCount imgs)

splitTest :: Bool -> IO ()
splitTest doFiltering = do
    putStrLn "Enter a divisor"
    divisor <- readLn
    putStrLn "Enter a threshold"
    t <- readLn
    tryBigTest (\img -> do
        display img
        testSplitting img
        splitRecurse $ reverse $ sortOn symbolWeight $ (if doFiltering then filter (filterFunc) else id) $ imageToSymbols img
        ) divisor t
    where splitRecurse imgs = do
            putStrLn "Show (n) next symbols: "
            nextImageCount <- readLn
            if nextImageCount == 0 then return () else do
                sequence $ Prelude.map (display . symbolImage) (take nextImageCount imgs)
                splitRecurse (drop nextImageCount imgs)

filterFunc :: Symbol -> Bool
filterFunc = do
    long <- filterFunc_long
    small <- filterFunc_small
    return (long && small)

filterFunc_long :: Symbol -> Bool
filterFunc_long = (\x-> maximum x < 3*(minimum x) ). (<*>) [fst,snd] . pure . symbolDims

filterFunc_small :: Symbol -> Bool
filterFunc_small = ((<) 10) . symbolWeight

displayOps :: [(String, IO ())] -> String
displayOps = disp 0 
    where disp n [] = ""
          disp n (x:xs) = (show n) ++ ". " ++ (fst x) ++ "\n" ++ (disp (succ n) xs)
