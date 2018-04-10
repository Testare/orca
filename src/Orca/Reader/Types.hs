module Orca.Reader.Types where

import Graphics.Image(Image, VS, X, Y, RGB, Bit, dims)
import Data.Ratio(Ratio, (%))
import Data.Char(isUpper)
import Numeric.LinearAlgebra(Vector)
import qualified Data.Map as M

type BitImage = Image VS X Bit
type GrayImage = Image VS Y Double
type EigenFace = ((Int, Int), Vector Double)
type ColorImage = Image VS RGB Double

type Dataset cs e = (DatasetType, M.Map String (Image VS cs e))
data DatasetType = Alpha | Eigen | Zeta deriving(Eq,Show)

type AlphaData = M.Map String [BitImage]
type EigenData = M.Map String [GrayImage]
type ZetaData = M.Map String ColorImage

data TDataset = TDataset 
                    { alpha :: AlphaData
                    , eigen :: EigenData
                    , zeta :: ZetaData
                    }

instance Monoid TDataset where
    mempty = TDataset M.empty M.empty M.empty
    mappend (TDataset a e z) (TDataset a' e' z') =  TDataset (M.unionWith mappend a a') (M.unionWith mappend e e') (M.union z z')

data ClassificationMethod = SimpleClassification | PCAClassifiction deriving (Read, Show)

type SymbolName = [Char]

symbolNameLength = 4
isValidSymbolName :: [Char] -> Bool
isValidSymbolName = (==) symbolNameLength . length

stringToSymbolName :: [Char] -> SymbolName
stringToSymbolName str = take (succ symbolNameLength) $ fstChr:str ++ (repeat '_')
    where fstChr = if isUpper $ head str then '^' else '_'

data Symbol = Symbol
    { symbolDims :: (Int, Int) --height, width
    , symbolOffset :: (Int, Int)
    , symbolImage :: BitImage
    , symbolWeight :: Int
    , symbolDensity :: Ratio Int
    }

bitimageToBasicSymbol :: BitImage -> Symbol
bitimageToBasicSymbol bi = Symbol 
                        { symbolDims = dims bi
                        , symbolOffset = (0,0)
                        , symbolImage = bi
                        , symbolWeight = 0 --Just some random default
                        , symbolDensity = 1 % 1 --Just some random default
}


data Params = Params
    { paramPercentRadius :: Double
    , paramThreshold :: Int
    , paramFiltering :: Bool
    , paramClassificationMethod :: ClassificationMethod
    , paramImageComparisonDims :: (Int, Int)
    , paramEuclideanDistance :: Bool
    }
defaultParams :: Params
defaultParams = Params 
                    { paramPercentRadius = 5
                    , paramThreshold = 5
                    , paramFiltering = True
                    , paramClassificationMethod = SimpleClassification
                    , paramImageComparisonDims = (50,50)
                    , paramEuclideanDistance = False
                    }

instance Show Symbol where
    show (Symbol (h,w) (x,y) _ weight density) = (show w) ++ 'x':(show h) ++ ' ':(show (x,y)) ++ ' ':'w':':':(show weight) ++ ' ':'D':':':(show density)