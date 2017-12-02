module Orca.Reader.Greyscale where
import Graphics.Image.Interface hiding (map)
import Graphics.Image hiding (map)
import qualified Graphics.Image as Hip (map)
import Graphics.Image.ColorSpace
import Graphics.Image.IO
import Data.Map as M
import Data.Maybe
import Data.Vector.Storable as V
import Data.Int(Int64)

data ThresholdOp = BinaryImage | CutOff deriving (Show,Read)

tryWithGrey :: Readable (Image VS RGB Double) format => String -> format -> (Image VS Y Double -> IO ()) -> IO ()
tryWithGrey filepath format f = do
    image <- (readImageExact format filepath) :: IO (Either String (Image VS RGB Double))
    either (putStrLn . (mappend "Error with file: ")) (f . toImageY) image

hSumVector :: (Storable a, Integral a) => Int -> V.Vector a -> V.Vector Int64
hSumVector n v = constructN (V.length v) subF
    where subF subV
            | len `mod` n == 0 = val
            | otherwise = val + (V.last subV)
            where len = V.length subV
                  val = fromIntegral (v V.! len)

vSumVector :: Int -> V.Vector Int64 -> V.Vector Int64
vSumVector n v = constructN (V.length v) subF
    where subF subV
            | len `div` n == 0 = val
            | otherwise = val + (subV V.! (len - n))
            where len = V.length subV
                  val = (v V.! len)

getIntegralVector :: Image VS Y Double -> V.Vector Int64
getIntegralVector img = vSumVector w $ hSumVector w $ V.map toNum imgV
    where (h,w) = dims img
          imgV = toVector img
          toNum = toWord8 . Prelude.sum . toListPx

getTotalValue :: Image VS Y Double -> Int64
getTotalValue img = V.foldl (\x y -> if ((x + y) < x) then error "It loops back" else (x + y)) 0 $ V.map toNum imgV
    where imgV = toVector img
          toNum = fromIntegral . toWord8 . Prelude.sum . toListPx

pixelThresholdStats :: Int -> Int -> (Int, Int) -> V.Vector Int64 -> Int -> String
pixelThresholdStats sr w (h1,w1) intVec i = stats
    where getSumAt (x,y) = intVec V.! (y*w+x)
          x = i `mod` w
          y = i `div` w
          x1 = max (x - sr) 0
          y1 = max (y - sr) 0
          x2 = min (x + sr) w1
          y2 = min (y + sr) h1
          count = fromIntegral $ (x2 - x1) * (y2 - y1)
          {-sum_ = Prelude.sum 
            [ (getSumAt (x2,y2))
            , (getSumAt (x1,x1))
            , (-1 * (getSumAt (x1,y2)))
            , (-1 * (getSumAt (x2,y1))) ]-}
          sum_ = (getSumAt (x1,y1)) + (getSumAt (x2,y2)) - (getSumAt (x2,y1)) - (getSumAt (x1,y2))
          stats = Prelude.concat [ show x
                  , ","
                  , show y
                  , ","
                  , show x1
                  , ","
                  , show y1
                  , ","
                  , show x2
                  , ","
                  , show y2
                  , ";"
                  , show $ getSumAt (x1,y1)
                  , ","
                  , show $ -1 * (getSumAt (x2,y1))
                  , ","
                  , show $ -1 * (getSumAt (x1,y2))
                  , ","
                  , show $ getSumAt (x2,y2)
                  , ";"
                  , show count
                  , ","
                  , show sum_
                  , ","
                  , show (fromIntegral $ (div sum_ count))
                  , ","
                  , show (fromIntegral $ (div sum_ count) :: Word8)
                  , ","
                  , show $ toDouble $ (fromIntegral $ (div sum_ count) :: Word8)
                  ]



pixelThreshold :: Int -> Int -> (Int, Int) -> V.Vector Int64 -> Int -> Pixel Y Double
pixelThreshold sr w (h1,w1) intVec i = PixelY $ toDouble $ (fromIntegral $ (div sum_ count) :: Word8)
    where getSumAt (x,y) = intVec V.! (y*w+x)
          x = i `mod` w
          y = i `div` w
          x1 = max (x - sr) 0
          y1 = max (y - sr) 0
          x2 = min (x + sr) w1
          y2 = min (y + sr) h1
          count = fromIntegral $ (x2 - x1) * (y2 - y1)
          {-sum_ = Prelude.sum 
            [ (getSumAt (x2,y2))
            , (getSumAt (x1,x1))
            , (-1 * (getSumAt (x1,y2)))
            , (-1 * (getSumAt (x2,y1))) ]-}
          sum_ = (getSumAt (x1,y1)) + (getSumAt (x2,y2)) - (getSumAt (x2,y1)) - (getSumAt (x1,y2))

pixelThreshold2 :: Int64 -> Int -> Int -> (Int, Int) -> V.Vector Int64 -> Int -> Pixel Y Double
pixelThreshold2 tv sr w (h1,w1) intVec i = PixelY $ toDouble $ (fromIntegral $ (div ((*) 255 $ getSumAt (x,y)) tv) :: Word8)--(div sum_ count) :: Word8)
    where getSumAt (x,y) = intVec V.! (y*w+x)
          x = i `mod` w
          y = i `div` w
          x1 = max (x - sr) 0
          y1 = max (y - sr) 0
          x2 = min (x + sr) w1
          y2 = min (y + sr) h1
          count = fromIntegral $ (x2 - x1) * (y2 - y1)
          sum_ = (* 255) $ Prelude.sum 
            [ getSumAt(x2,y2) 
            , getSumAt(x1,x1) 
            , -getSumAt(x1,y2) 
            , -getSumAt(x2,y1) ]

getThresholdImg :: Int -> Image VS Y Double -> Image VS Y Double
getThresholdImg sr img = fromVector (h,w) $ V.generate (V.length intVec) pt 
    where intVec = getIntegralVector img
          (h,w) = dims img
          w1 = pred w
          h1 = pred h
          tv = getTotalValue img
          pt = (pixelThreshold{-2 tv-} sr w (h1,w1) intVec)
          --pixelThreshold i = PixelY $ toDouble $ (div sum_ count)

-- Pass in the t threshold value, then the threshold image, then the image
-- 0.0 <= t <= 100.0
adaptiveThreshold :: Double -> Image VS Y Double -> Image VS Y Double -> Image VS X Bit
adaptiveThreshold t = thresholdWith2 appPx
    where appPx = (PixelY $ (\intImg img -> img <= intImg*((100.0 - t)/100.0))) :: Pixel Y (Double -> Double -> Bool)

adaptiveThresholdRatio :: Int -> Double -> Image VS Y Double -> Image VS X Bit
adaptiveThresholdRatio divisor t img = adaptiveThreshold t intImg img
    where (h,w) = dims img
          sr = div (min h w) divisor
          intImg = getThresholdImg sr img

threePxLine :: Image VS X Bit
threePxLine = fromVector (3,1) $ generate 3 (\i -> on)

twoPxLine :: Image VS X Bit
twoPxLine = fromVector (2,1) $ generate 2 (\i -> on)

adaptThresholdPart3 :: Image VS X Bit -> Image VS X Bit
adaptThresholdPart3 = open threePxLine

adaptThresholdPart32 :: Image VS X Bit -> Image VS X Bit
adaptThresholdPart32 = open twoPxLine