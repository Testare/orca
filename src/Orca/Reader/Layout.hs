module Orca.Reader.Layout
    ( Symbol
    , symbolDims
    , symbolOffset 
    , symbolImage 
    , symbolWeight 
    , symbolDensity
    , imageToSymbols
    , overlaySymbolCoverages
    ) where
import Orca.Reader.Layout.ParseSymbols
import Orca.Reader.Types

import Data.Tuple(swap)

import Graphics.Image(Image, VS, X, Bit, off, on, dims, isOn, superimpose, fromImagesX)
import Graphics.Image.Interface(Vector, Pixel, Array, toVector, fromVector, fromIx, new, freeze, write, promote, makeImage, toDouble, Elevator)
import qualified Graphics.Image.Interface as HIPI(map)

binaryToGrayscale :: Double -> Image VS X Bit -> Image VS X Double
binaryToGrayscale value = HIPI.map (\x -> if isOn x then valPx else zero) 
        where valPx = promote value
              zero = promote 0.0

{-BUG: Symbols that overlap in general area will erase parts of other symbols they overlap-}
symbolCoverageImage :: (Elevator e, Array arr x e) => Image arr x e -> Double -> [Symbol] -> Image VS X Double
symbolCoverageImage img value symbols = foldl (\accImg sym -> superimpose (swap $ symbolOffset sym) (getSymbolImage sym) accImg) baseImage symbols
    where baseImage = makeImage (dims img) (const $ promote 0.0) 
          getSymbolImage = binaryToGrayscale value . symbolImage

overlaySymbolCoverages :: (Elevator e, Array VS cs e, Array VS cs Double) => Image VS X Bit -> Double -> cs -> [([Symbol], e, cs)] -> Image VS cs Double
overlaySymbolCoverages img baseVal baseColor symbolList = fromImagesX ((baseColor, baseImage):otherXs)
    where baseImage = binaryToGrayscale baseVal img 
          otherXs = map (\(symbols, e, cs) -> (cs, symbolCoverageImage img (toDouble e) symbols)) symbolList
