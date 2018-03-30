module Orca.Reader.Types where

import Graphics.Image(Image, VS, X, Y, RGB, Bit)
import Data.Ratio(Ratio)
import qualified Data.Map as M

type Dataset cs e = (DatasetType, M.Map String (Image VS cs e))
data DatasetType = AlphaData | EigenData | ZetaData deriving(Eq)
data TDataset = TDataset 
                    { alpha :: M.Map String [BitImage]
                    , eigen :: M.Map String [GrayImage]
                    , zeta :: M.Map String ColorImage
                    }
instance Monoid TDataset where
    mempty = TDataset M.empty M.empty M.empty
    mappend (TDataset a e z) (TDataset a' e' z') =  TDataset (M.unionWith mappend a a') (M.unionWith mappend e e') (M.union z z')

data ClassificationMethod = SimpleClassification | PCAClassifiction deriving (Read, Show)

type SymbolName = [Char]

type BitImage = Image VS X Bit
type GrayImage = Image VS Y Double
type ColorImage = Image VS RGB Double

symbolNameLength = 4
isValidSymbolName :: [Char] -> Bool
isValidSymbolName = (==) symbolNameLength . length

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