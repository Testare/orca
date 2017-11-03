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

{-
constructIntVector :: Image VS Y Double -> Vector Int64
constructIntVector img = V.constructN (V.length imgV) vectorF
    where dim@(h,w) = dims img
          imgV = toVector img
          vectorF n = (Prelude.map subVectorF [0..] !!)
            where subVectorF
                  acc = 
                  val = fromIntegral $ toWord8 $ foldrpx (+) (imvV ! n)-}
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
          sum_ = Prelude.sum 
            [ getSumAt(x2,y2) 
            , getSumAt(x1,x1) 
            , -getSumAt(x1,y2) 
            , -getSumAt(x2,y1) ]

getThresholdImg :: Int -> Image VS Y Double -> Image VS Y Double
getThresholdImg sr img = fromVector (h,w) $ V.generate (V.length intVec) (pixelThreshold sr w (h1,w1) intVec)
    where intVec = getIntegralVector img
          (h,w) = dims img
          w1 = pred w
          h1 = pred h
          --pixelThreshold i = PixelY $ toDouble $ (div sum_ count)

adaptiveThreshold ::  Image VS Y Double -> Image VS Y Double
adaptiveThreshold img = fromVector dim $ toVector img
    where dim@(h,w) = dims img
          imgV = toVector img
          intToCoords n = (n `mod` w, n `div` w)
                    {-g v = xmid
                    where xmid = (V.length v) `mod` w
                          ymid = (V.length v) `div` w
                          x1 = max (xmid - sr) 0
                          x2 = min (xmid + sr) (w - 1)
                          y1 = max (ymid - sr) 0
                          y2 = min (ymid + sr) (h - 1)-}
                          

fib = (Prelude.map f [0..] !!)
    where f 0 = 0
          f 1 = 0
          f n = (fib (pred n)) + (fib (n - 2))

           
    {-
createHistogram :: Image Pixel8 -> M.Map Pixel8 Int

histogramToList :: M.Map Pixel8 Int -> [Int]
histogramToList mp = fromMaybe 0 . flip M.lookup mp <$> ([minBound..maxBound] :: [Pixel8])

threshold :: ThresholdOp -> Pixel8 -> Pixel8 -> Image Pixel8 -> Image Pixel8
threshold BinaryImage low high = pixelMap (\x-> if (x > low) && (x < high) then 0 else 255)
threshold CutOff low high = pixelMap (\x-> if (x > low) && (x < high) then x else 255)

{-Code to eventually remove-}
old_grayScalePx :: PixelRGB8 -> PixelRGB8
old_grayScalePx (PixelRGB8 r g b ) = PixelRGB8 gray gray gray
    where gray = fromIntegral $ div ((fromIntegral r)*299 + (fromIntegral g)*587 + (fromIntegral b)*114) (1000 :: Int)
    -}