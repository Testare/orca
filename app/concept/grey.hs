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
      , ("Test splitting", splitTest)
      , ("Test second splitting", testSplitting2)
      , ("Test filtered splitting", testFilteredSplitting)
      ]

testFilteredSplitting :: IO ()
testFilteredSplitting = putStrLn "Coming soon!"

splitTest :: IO ()
splitTest = do
    putStrLn "Enter a divisor"
    divisor <- readLn
    putStrLn "Enter a threshold"
    t <- readLn
    tryBigTest (\img -> do
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

displayOps :: [(String, IO ())] -> String
displayOps = disp 0 
    where disp n [] = ""
          disp n (x:xs) = (show n) ++ ". " ++ (fst x) ++ "\n" ++ (disp (succ n) xs)
