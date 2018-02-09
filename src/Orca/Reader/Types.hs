module Orca.Reader.Types where

import Graphics.Image(Image, VS, X, Bit)
import Data.Ratio(Ratio)

data ClassificationMethod = SimpleClassification | PCAClassifiction deriving (Read, Show)

data Symbol = Symbol
    { symbolDims :: (Int, Int) --height, width
    , symbolOffset :: (Int, Int)
    , symbolImage :: Image VS X Bit
    , symbolWeight :: Int
    , symbolDensity :: Ratio Int
    }

data Params = Params
    { paramPercentRadius :: Double
    , paramThreshold :: Int
    , paramFiltering :: Bool
    , paramClassificationMethod :: ClassificationMethod
    , paramImageComparisonDims :: (Int, Int)
    }

instance Show Symbol where
    show (Symbol (h,w) (x,y) _ weight density) = (show w) ++ 'x':(show h) ++ ' ':(show (x,y)) ++ ' ':'w':':':(show weight) ++ ' ':'D':':':(show density)