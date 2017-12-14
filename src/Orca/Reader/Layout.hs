module Orca.Reader.Layout
    ( Symbol
    , symbolDims
    , symbolOffset 
    , symbolImage 
    , symbolWeight 
    , symbolDensity
    , imageToSymbols
    , testSplitting
    , testSplitting2
    , testMap
    , getSymbolImages
    , overlaySymbolCoverages
    ) where

import Data.Maybe
import Data.List as L
import Data.Ratio
import Data.Tuple(swap)

import Graphics.Image(Image, VS, X, Bit, off, on, dims, isOn, superimpose, fromImagesX)
import Graphics.Image.Interface(Vector, Pixel, Array, toVector, fromVector, fromIx, new, freeze, write, promote, makeImage, toDouble, Elevator)
import qualified Graphics.Image.Interface as HIPI(map)
import Control.Monad.ST(runST)
import qualified Data.Vector.Storable as V
import qualified Data.Map as M

-- REMOVE SHOW and debugging stuff. Reorganize. Use STData

{-- Public interface -}
binaryToGrayscale :: Double -> Image VS X Bit -> Image VS X Double
binaryToGrayscale value = HIPI.map (\x -> if isOn x then valPx else zero) 
        where valPx = promote value
              zero = promote 0.0


imageToSymbols :: Image VS X Bit -> [Symbol]
imageToSymbols = map symbolFromData . stripsToSymbolData . splitImageToStrips

symbolCoverageImage :: (Elevator e, Array arr x e) => Image arr x e -> Double -> [Symbol] -> Image VS X Double
symbolCoverageImage img value symbols = foldl (\accImg sym -> superimpose (swap $ symbolOffset sym) (getSymbolImage sym) accImg) baseImage symbols
    where baseImage = makeImage (dims img) (const $ promote 0.0) 
          getSymbolImage = binaryToGrayscale value . symbolImage

overlaySymbolCoverages :: (Elevator e, Array VS cs e, Array VS cs Double) => Image VS X Bit -> Double -> cs -> [([Symbol], e, cs)] -> Image VS cs Double
overlaySymbolCoverages img baseVal baseColor symbolList = fromImagesX ((baseColor, baseImage):otherXs)
    where baseImage = binaryToGrayscale baseVal img 
          otherXs = map (\(symbols, e, cs) -> (cs, symbolCoverageImage img (toDouble e) symbols)) symbolList

{-Symbol Type-}

data Symbol = Symbol
    { symbolDims :: (Int, Int) --height, width
    , symbolOffset :: (Int, Int)
    , symbolImage :: Image VS X Bit
    , symbolWeight :: Int
    , symbolDensity :: Ratio Int
    }

instance Show Symbol where
    show (Symbol (h,w) (x,y) _ weight density) = (show w) ++ 'x':(show h) ++ ' ':(show (x,y)) ++ ' ':'w':':':(show weight) ++ ' ':'D':':':(show density)

symbolFromData :: SymbolData -> Symbol
symbolFromData symbolData@(SymbolData _ minX_ maxX_ minY_ maxY_ weight_) = Symbol 
    { symbolDims = (h, w)
    , symbolOffset = (minX_, minY_)
    , symbolImage = symbolDataToImage symbolData
    , symbolWeight = weight_
    , symbolDensity = weight_ % (h*w)
    }
    where w = succ $ maxX_ - minX_
          h = succ $ maxY_ - minY_

symbolDataToImage :: SymbolData -> Image VS X Bit 
symbolDataToImage (SymbolData strips_ minX_ maxX_ minY_ maxY_ weight_) = sDTI_coordsToImage h w $ sDTI_stripsToCoords (minX_, minY_) strips_
    where w = succ $ maxX_ - minX_
          h = succ $ maxY_ - minY_

sDTI_stripsToCoords :: (Int, Int) -> [YStrip] -> [(Int, Int)]
sDTI_stripsToCoords (offX, offY) = concat . map decompose
    where decompose (KeyStrip x xMax y) = map (\x -> (y - offY, x-offX)) [x..xMax]
    
sDTI_coordsToImage :: Int -> Int -> [(Int, Int)] -> Image VS X Bit
sDTI_coordsToImage h w coords = runST $ do
    img <- new (h, w) --Out of order?
    sequence $ map (\x -> write img x on) coords
    freeze img

{- version 2-}

{-Strip basics-}
data BaseStrip = BaseStrip Int Int deriving (Show)
data KeyStrip a = KeyStrip Int Int a deriving (Show, Eq)
type YStrip = KeyStrip Int

instance (Ord a) => Ord (KeyStrip a) where
    compare (KeyStrip b _ a) (KeyStrip b1 _ a1) = if primComp == EQ then compare b b1 else primComp
        where primComp = compare a a1

class Strip a where 
    stripMin :: a -> Int
    stripMax :: a -> Int
    stripWidth :: a -> Int
    baseStrip :: a -> BaseStrip
    baseStrip a = BaseStrip (stripMin a) (stripMax a)
    stripWidth a = succ $ (stripMax a) - (stripMin a)

instance Strip BaseStrip where
    stripMin (BaseStrip a _ ) = a
    stripMax (BaseStrip _ a ) = a
    baseStrip = id

instance Strip (KeyStrip k) where
    stripMin (KeyStrip a _ _) = a
    stripMax (KeyStrip _ a _) = a

stripKey :: KeyStrip a -> a
stripKey (KeyStrip _ _ c) = c

swapKey :: KeyStrip a -> b -> KeyStrip b
swapKey (KeyStrip min_ max_ _) = KeyStrip min_ max_

checkStripCollision :: (Strip a, Strip b) => a -> b -> Bool
checkStripCollision a b = min1 <= max2 && min2 <= max1
    where min1 = stripMin a
          min2 = stripMin b
          max1 = stripMax a
          max2 = stripMax b
        
{-SymbolData basics-}

imageToSymbolData :: Image VS X Bit -> [SymbolData]
imageToSymbolData = stripsToSymbolData . splitImageToStrips

data SymbolData = SymbolData
                    { strips :: [YStrip]
                    , minX :: Int
                    , maxX :: Int
                    , minY :: Int
                    , maxY :: Int
                    , weight :: Int
                    } deriving (Show)

instance Eq SymbolData where
    a == b = (weight a) == (weight b)

instance Ord SymbolData where
    a <= b = (weight a) <= (weight b)
    a < b = (weight a) < (weight b)
    compare a b = compare (weight a) (weight b)

instance Monoid SymbolData where
    mempty = SymbolData [] maxBound minBound maxBound minBound 0
    mappend sym1 sym2 = SymbolData
                    { strips = concat $ map strips syms 
                    , minX = minimum $ map minX syms
                    , maxX = maximum $ map maxX syms
                    , minY = minimum $ map minY syms
                    , maxY = maximum $ map maxY syms
                    , weight = sum $ map weight syms
                    }
                    where syms = [sym1, sym2]

initializeSymbolFromStrip :: YStrip -> SymbolData
initializeSymbolFromStrip = do
    ss <- pure
    minX_ <- stripMin
    maxX_ <- stripMax
    y <- stripKey
    weight <- stripWidth
    return $ SymbolData ss minX_ maxX_ y y weight

appendStripToSymbolData :: YStrip -> SymbolData -> SymbolData
appendStripToSymbolData strip symbolData = symbolData 
                        { strips = strip:ss
                        , minX = min minX1 (stripMin strip)
                        , maxX = max maxX1 (stripMax strip)
                        , maxY = max maxY1 (stripKey strip)
                        , weight = w1 + (stripWidth strip)
                        }
                        where (SymbolData 
                                { strips = ss
                                , minX = minX1
                                , maxX = maxX1
                                , maxY = maxY1
                                , weight = w1}) = symbolData

{- Image -> Strips/STData -> SymbolData -> Symbol Conversion Process -}

-- Use this later to make the type signatures more legible
type STData a = (a, [KeyStrip a], M.Map a SymbolData)

splitImageToStrips :: Image VS X Bit -> [[YStrip]]
splitImageToStrips img = vectorToStrips 0 $ V.map isOn imgV
    where (h,w) = dims img
          imgV = toVector img
          vectorToStrips y vec
            | V.null vec = []
            | otherwise = (splitImageToStrips_convertVectorToStrips y fst_):(vectorToStrips (succ y) snd_)
            where (fst_,snd_) = V.splitAt w vec 

stripsToSymbolData :: [[YStrip]] -> [SymbolData]
stripsToSymbolData = M.elems . (\(_,_,x) -> x) . foldl stripsToSymbolData_rowFolding (0 :: Int, mempty, mempty)

splitImageToStrips_convertVectorToStrips :: Int -> V.Vector Bool -> [YStrip]
splitImageToStrips_convertVectorToStrips y vec = maybe ss (flip (:) ss) result
    where myFunc :: (Int, [YStrip], Maybe YStrip) -> Bool -> (Int, [YStrip], Maybe YStrip)
          myFunc (x, ss, Nothing) True = (succ x, ss, Just (KeyStrip x x y))
          myFunc (x, ss, Nothing) False = (succ x, ss, Nothing)
          myFunc (x, ss, Just (KeyStrip xm _ _)) True = (succ x, ss, Just (KeyStrip xm x y))
          myFunc (x, ss, Just lastStrip) False = (succ x, lastStrip:ss, Nothing)
          (_, ss, result) = V.foldl myFunc (0, [], Nothing) vec


-- To optimize in the future: Redo stripsToData to work backwards, and remove "reverse" from this function
stripsToSymbolData_rowFolding :: (Show a, Enum a, Ord a) => (a, [KeyStrip a], M.Map a SymbolData) -> [YStrip] -> (a, [KeyStrip a], M.Map a SymbolData)
stripsToSymbolData_rowFolding sData@(a, keys, dataMap) ystrip = (nextA, (reverse nextKeys), nextMap)
                            where foldCollect :: (Show a, Enum a, Ord a) => ([KeyStrip a], (a, [KeyStrip a], M.Map a SymbolData)) -> YStrip -> ([KeyStrip a], (a, [KeyStrip a], M.Map a SymbolData))
                                  foldCollect (keyss, sData_) ystr = let (keystrip, newData) = stripsToSymbolData_cellFolding sData_ ystr in (keystrip:keyss, newData)
                                  (nextKeys, (nextA, _, nextMap)) = foldl foldCollect ([], sData) (reverse ystrip)

stripsToSymbolData_cellFolding :: (Show a, Enum a, Ord a) => (a, [KeyStrip a], M.Map a SymbolData) -> YStrip -> (KeyStrip a, (a, [KeyStrip a], M.Map a SymbolData))
stripsToSymbolData_cellFolding (a, [], dataMap) ystrip = (swapKey ystrip a, (succ a, [], M.insert a (initializeSymbolFromStrip ystrip) dataMap))
stripsToSymbolData_cellFolding (a, keys@(keysHead:keysTail), dataMap) ystrip
    | stripMax keysHead < stripMin ystrip = stripsToSymbolData_cellFolding (a, keysTail, dataMap) ystrip
    | stripMax ystrip < stripMin keysHead = (swapKey ystrip a, (succ a, keys, M.insert a (initializeSymbolFromStrip ystrip) dataMap))
    | otherwise = (swapKey ystrip headKey, (a, nextKeys, M.adjust (appendStripToSymbolData ystrip) headKey nextMap))
    where toMerge = map stripKey $ takeWhile (checkStripCollision ystrip) keysTail
          headKey = stripKey keysHead
          (nextKeys, nextMap) = foldl (stripsToSymbolData_mergeKeys headKey) (drop (length toMerge) keys, dataMap) (if True || toMerge == [] then toMerge else (error $ "headkey:" ++ (show headKey) ++ "\nkeys" ++ (show keys) ++ "\nmap" ++ (show dataMap) ++  "\n - " ++ (show toMerge)) :: (Show a) =>[a])

-- Not safe when newKey == oldKey
stripsToSymbolData_mergeKeys :: (Ord a) => a -> ([KeyStrip a], M.Map a SymbolData) -> a -> ([KeyStrip a], M.Map a SymbolData)
stripsToSymbolData_mergeKeys newKey mapState@(keystrips, oldMap) oldKey 
    | newKey == oldKey = mapState
    | otherwise = (correctedStrips, updatedMap)
    where oldSymbolData = M.findWithDefault mempty oldKey oldMap
          updatedMap = M.delete oldKey $ M.adjust (mappend oldSymbolData) newKey oldMap
          correctedStrips = flip map keystrips $ \keystrip-> if (stripKey keystrip) == oldKey then swapKey keystrip newKey else keystrip

{-TEST STUFF-}

testSplitting2 :: IO ()
testSplitting2 = do
    let testSymbolData = stripsToSymbolData testStrips1
    putStrLn $ show $ testSymbolData
    putStrLn $ (++) "Symbols: " $ show $ length $ testSymbolData
    putStrLn $ (++) "Strips: " $ show $ length $ concat $ map strips testSymbolData
    putStrLn "Test ST"
    putStrLn $ show $ stripsToSymbolData_cellFolding testSTData (KeyStrip 0 2 2)
    putStrLn "Where it breaks"
    putStrLn $ show $ stripsToSymbolData_mergeKeys 2 ([KeyStrip 2 3 0, KeyStrip 6 6 3], testMap2) 0

testSplitting :: Image VS X Bit -> IO ()
--testSplitting = (\x -> x >> (return ())) . sequence . (map (putStrLn . show)) . (take 15) . splitImageToStrips
--testSplitting = (\x -> x >> (return ())) . putStrLn . concat . map show . imageToSymbols
testSplitting = (\x -> x >> (return ())) . sequence . (map (putStrLn . show)) . L.sort . imageToSymbolData
--testSplitting = (\x -> x >> (return ())) . putStrLn . show . length . splitImageToStrips

getSymbolImages :: Image VS X Bit -> [Image VS X Bit]
getSymbolImages = map symbolDataToImage . reverse . L.sort . imageToSymbolData

testMap :: ([KeyStrip Int], M.Map Int SymbolData)
testMap = stripsToSymbolData_mergeKeys 2 ([(KeyStrip 2 8 3), (KeyStrip 9 12 2), (KeyStrip 14 14 5)],(M.fromList 
                [ (2 :: Int, (initializeSymbolFromStrip (KeyStrip 1 3 0)))
                , (3 :: Int, (initializeSymbolFromStrip (KeyStrip 2 5 1)))
                ])) 3

testSTData :: (Int, [KeyStrip Int], M.Map Int SymbolData)
testSTData = (2 :: Int, keys, sd)
    where ys = [(0, KeyStrip 1 2 1), (1, KeyStrip 4 7 1)]
          sd = initializeSymbolFromStrip <$> M.fromList ys
          keys = map (\(key, strp)-> swapKey strp key) ys

testStrips1 :: [[YStrip]]
testStrips1 = 
    [ [(KeyStrip 4 5 0), (KeyStrip 1 2 0)]
    , [(KeyStrip 6 6 1), (KeyStrip 2 3 1), (KeyStrip 0 0 1)]
    , [(KeyStrip 4 6 2), (KeyStrip 0 2 2)]
    , [(KeyStrip 2 2 3), (KeyStrip 0 0 3)]
    ]

testMap2 :: M.Map Int SymbolData
testMap2 = M.fromList [(0,SymbolData {strips = [KeyStrip 2 3 1,KeyStrip 1 2 0], minX = 1, maxX = 3, minY = 0, maxY = 1, weight = 4}),(1,SymbolData {strips = [KeyStrip 4 5 0], minX = 4, maxX = 5, minY = 0, maxY = 0, weight = 2}),(2,SymbolData {strips = [KeyStrip 0 0 1], minX = 0, maxX = 0, minY = 1, maxY = 1, weight = 1}),(3,SymbolData {strips = [KeyStrip 6 6 1], minX = 6, maxX = 6, minY = 1, maxY = 1, weight = 1})]
