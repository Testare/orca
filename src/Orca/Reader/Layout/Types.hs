module Orca.Reader.Layout.Types where

import Graphics.Image(Image, VS, X, Bit)
import Data.Ratio(Ratio)

data Symbol = Symbol
    { symbolDims :: (Int, Int) --height, width
    , symbolOffset :: (Int, Int)
    , symbolImage :: Image VS X Bit
    , symbolWeight :: Int
    , symbolDensity :: Ratio Int
    }


instance Show Symbol where
    show (Symbol (h,w) (x,y) _ weight density) = (show w) ++ 'x':(show h) ++ ' ':(show (x,y)) ++ ' ':'w':':':(show weight) ++ ' ':'D':':':(show density)