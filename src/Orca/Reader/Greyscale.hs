module Orca.Reader.Greyscale where
import Codec.Picture
import Codec.Picture.Types
import Data.Map as M
import Data.Maybe

data ThresholdOp = BinaryImage | CutOff deriving (Show,Read)

grayScalePx :: PixelRGB8 -> Pixel8
grayScalePx (PixelRGB8 r g b ) = gray
    where gray = fromIntegral $ div ((fromIntegral r)*299 + (fromIntegral g)*587 + (fromIntegral b)*114) (1000 :: Int)

convertToGreyscale :: Image PixelRGB8 -> Image Pixel8
convertToGreyscale = pixelMap grayScalePx

createHistogram :: Image Pixel8 -> M.Map Pixel8 Int
createHistogram = pixelFold f mempty 
    where f histomap _ _ px = M.insertWith (return succ) px 1 histomap

histogramToList :: M.Map Pixel8 Int -> [Int]
histogramToList mp = fromMaybe 0 . flip M.lookup mp <$> ([minBound..maxBound] :: [Pixel8])

threshold :: ThresholdOp -> Pixel8 -> Pixel8 -> Image Pixel8 -> Image Pixel8
threshold BinaryImage low high = pixelMap (\x-> if (x > low) && (x < high) then 0 else 255)
threshold CutOff low high = pixelMap (\x-> if (x > low) && (x < high) then x else 255)

{-Code to eventually remove-}
old_grayScalePx :: PixelRGB8 -> PixelRGB8
old_grayScalePx (PixelRGB8 r g b ) = PixelRGB8 gray gray gray
    where gray = fromIntegral $ div ((fromIntegral r)*299 + (fromIntegral g)*587 + (fromIntegral b)*114) (1000 :: Int)