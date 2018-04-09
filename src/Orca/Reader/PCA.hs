module Orca.Reader.PCA where 

import Orca.Reader.Types
import Graphics.Image(Pixel, X(..), Bit, Y)
import qualified Graphics.Image as HIP

--generateEigenFaces :: [BitImage] -> [GrayImage]
--generateEigenFaces = 

{- Helper functions -}
bitToGray :: HIP.Pixel HIP.X HIP.Bit -> HIP.Pixel HIP.Y Double 
bitToGray px
    | HIP.isOn px = HIP.PixelY 1.0
    | otherwise = HIP.PixelY 0.0
