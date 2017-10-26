module Orca.Reader.Clustering where

import System.Random
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import Control.Applicative
import Data.Int(Int64)
import Codec.Picture.Types
import Codec.Picture
import Data.Ratio
import GHC.Exts(sortWith)

{-Operational Parameters-}
data PixelMetric = Euclid2D | Euclid3D deriving (Show, Ord, Eq)
data AverageProcess = StraightAverage | WeightedAverage deriving (Show, Ord, Eq)
data Clustering = RandomPixels Int | Tricolor deriving (Show, Ord, Eq)
data WriteImageOp = GrayScale | MeanColor | Rainbow | Cluster Int deriving (Show, Ord, Eq)
data ImageClusterOp = ImageClusterOp PixelMetric AverageProcess Clustering [WriteImageOp] deriving (Show, Ord, Eq)

{-AVERAGE Monoid type-}
data Average a = Average { getTotal :: a
                       , getCount :: Int
                       } 

instance (Show a) => Show (Average a) where
    show (Average total cnt) = "A[" ++ (show total) ++ "/" ++ (show cnt) ++ "]"

instance (Num a) => Monoid (Average a) where
    mempty = Average 0 0
    mappend (Average tot1 cnt1) (Average tot2 cnt2) = Average (tot1+tot2) (cnt1+cnt2)

average :: (Integral a) => a -> Average a
average = flip Average 1

avgRatio :: (Integral a) => Average a -> Ratio a
avgRatio = do
    total <- getTotal
    cnt <- fromIntegral . getCount
    return $ (total % cnt)

{-AVERAGE PIXEL FUNCTIONS-}
newtype AveragePixel = AvgPx [Average Int64] 

instance Show AveragePixel where
    show (AvgPx avg) = flip (++) "}\n" $ (++) "APX{" $ concat $ map show avg

instance Monoid AveragePixel where
    mempty = AvgPx [mempty, mempty, mempty]
    mappend (AvgPx avgPx1) (AvgPx avgPx2) = AvgPx $ getZipList $ (\x y -> x <> y) <$> ZipList avgPx1 <*> ZipList avgPx2

weightAverage :: (Integral a) => Int -> Average a -> Average a
weightAverage weight newAvg@(Average total cnt) = Average ((fromIntegral weight)*total) (weight*cnt)

averagePixel :: AverageProcess -> Int ->  PixelRGB8 -> AveragePixel
averagePixel StraightAverage _ (PixelRGB8 r g b) = AvgPx $ average . fromIntegral <$> [r,g,b] 
averagePixel WeightedAverage weight (PixelRGB8 r g b) = AvgPx $ weightAverage weight . average . fromIntegral <$> [r,g,b]

pixelFromAverage :: AveragePixel -> PixelRGB8
pixelFromAverage (AvgPx pxLst) = PixelRGB8 r g b
    where [r,g,b] =(floor . avgRatio) <$> pxLst

{- MeanCluster Type-}
data MeanCluster = MeanCluster PixelRGB8 AveragePixel deriving Show
type KCluster = [MeanCluster]

seedPixelsToClusters :: [PixelRGB8] -> [MeanCluster]
seedPixelsToClusters = map (flip MeanCluster mempty)

clusterFold :: [MeanCluster] -> Image PixelRGB8 -> [MeanCluster]
clusterFold = pixelFold f
    where f acc _ _ px = fst $ addPixel (pixelDistance Euclid2D 0 0) acc px

addPixel :: (PixelRGB8 -> PixelRGB8 -> Int) -> [MeanCluster] -> PixelRGB8 -> ([MeanCluster], Int)
addPixel f [] pixel = ([], maxBound)
addPixel f [x@(MeanCluster meanPixel avg)] pixel = let dist = f meanPixel pixel 
                                                   in ([MeanCluster meanPixel $ avg <> (averagePixel WeightedAverage dist pixel)], dist)
addPixel f (x:xs) pixel 
    | snd a < snd b = (((fst a) ++ xs), snd a)
    | otherwise = (x:(fst b), snd b)
    where a = addPixel f [x] pixel
          b = addPixel f xs pixel

chewClusters :: [MeanCluster] -> [MeanCluster]
chewClusters = map f
    where f (MeanCluster _ avg) = (MeanCluster (pixelFromAverage avg) mempty)

{-Drawing-}
rainbowMap :: KCluster -> PixelRGB8 -> PixelRGB8
rainbowMap kcls = fromMaybe (PixelRGB8 0 0 0) . flip M.lookup rainMap
    where rainMap = mapTo kcls
    
mapTo :: KCluster -> M.Map PixelRGB8 PixelRGB8
mapTo kcls = M.fromList (zip kpxs pixels)
    where vals = [0x3F, 0x7F, 0xBF, 0xFF]
          pixels = PixelRGB8 <$> vals <*> vals <*> vals
          kpxs = map (\(MeanCluster px _) -> px) kcls

drawPixel :: KCluster -> PixelRGB8 -> PixelRGB8
drawPixel kcls nextPx = fst $ head $ sortWith snd distPxs
    where pxs = map (\(MeanCluster px _) -> px) kcls
          dist px = (px, pixelDistance Euclid3D 0 0 px nextPx)
          distPxs = map dist pxs

drawCluster :: Image PixelRGB8 -> KCluster -> Image PixelRGB8
drawCluster img kcs = pixelMap (drawPixel kcs) img
{- Do clustering -}

runKIterations :: Int -> KCluster -> Image PixelRGB8 -> KCluster
runKIterations 0 kcls _ = kcls
runKIterations n kcls img = runKIterations (pred n) nextKcls img
    where nextKcls = chewClusters $ clusterFold kcls img


kRandomClusters :: (RandomGen r) => Int -> r -> Image PixelRGB8 -> [MeanCluster]
kRandomClusters k rand img = seedPixelsToClusters $ take k $ randomPixels rand img

tryWithImage :: String -> (Image PixelRGB8 -> IO ()) -> IO ()
tryWithImage filepath f = do
    image <- readImage filepath
    either (putStrLn . (mappend "Error with file: ")) f (convertRGB8 <$> image)

{-Adjustable parameters-}

pixelDistance :: PixelMetric -> Int -> Int -> PixelRGB8 -> PixelRGB8 -> Int
pixelDistance Euclid2D _ _ = pixelDistance1
pixelDistance Euclid3D _ _ = pixelDistance3

pixelDistance1 (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = (diff r1 r2) + (diff g1 g2) + (diff b1 b2)
        where diff a b = ((fromIntegral b) - (fromIntegral a))^2

pixelDistance3 (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = (diff r1 r2) + (diff g1 g2) + (diff b1 b2)
        where diff a b = abs $ (((fromIntegral b) - (fromIntegral a)))^3



randomPixels ::  (RandomGen r) => r -> Image PixelRGB8 -> [PixelRGB8]
randomPixels rand img = getZipList $ pixelAt img <$> xGen <*> yGen 
    where xGen = ZipList $ randomRs (0, imageWidth img) rand
          yGen = ZipList $ randomRs (0, imageHeight img) rand

{- Failed distance metrics

-- Produces noise X[
pixelDistance4 (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = (diff (floor k*r1) r2) + (diff (floor k*g1) g2) + (diff (floor k*b1) b2)
        where k = (r1*r2 + g1*g2 + b1*b2)%(2*(r1*r1+g1*g1+b1*b1))
              diff a b = ((fromIntegral b) - (fromIntegral a))^2

pixelDistance5 (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = 2*(diff r1 r2) + 4*(diff g1 g2) + 3*(diff b1 b2)
        where diff a b = ((fromIntegral b) - (fromIntegral a))^2


pixelDistance6 (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = (diff r1 r2) + (diff g1 g2) + (diff b1 b2)
        where diff a b =  (x - y) * (x - y) * (x - y)
                where x = fromIntegral a
                      y = fromIntegral b

        -- Dunt werk
pixelDistance2 (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = (diff r1 r2) + (diff g1 g2) + (diff b1 b2)
        where diff a b = abs $ (fromIntegral a) - (fromIntegral b)

pixelDistanceByShade (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = diff (r1+g1+b1) (r2+g2+b2)
        where diff a b = ((fromIntegral b) - (fromIntegral a))^2
-}

{-runClusterOnImage :: String -> Int -> IO ()
runClusterOnImage imageSource n = do
    image <- readImage imageSource :: IO (Either String DynamicImage)-- Either String DynamicImage
    flip (either (putStrLn . (++ "Error: "))) (convertRGB8 <$> image) $ do
        report <- kClusterPixels n colorClusterReport
        drawCluster <- drawClusterImage
        let (KMeansPixelReport kMeans) = report
            (KMeansPixelReport bKMeans) = binaryReport
            whiteImage = drawCluster (bKMeans !! 0)
            --blackImage = drawCluster (kMeans !! 1)
            redImage = drawCluster (kMeans !! 0)
            greenImage = drawCluster (kMeans !! 1)
            blueImage = drawCluster (kMeans !! 2)
        return $ do
            putStrLn "Processing..."
            putStrLn $ (++) "Red Average: " $ show $ fromPixel $ averagePixel (kMeans !! 0)
            putStrLn $ (++) "Green Average: " $ show $ fromPixel $ averagePixel (kMeans !! 1)
            putStrLn $ (++) "Blue Average: " $ show $ fromPixel $ averagePixel (kMeans !! 2)
            putStrLn $ (++) "White Average: " $ show $ fromPixel $ averagePixel (bKMeans !! 0)
            putStrLn "Drawing red!"
            writePng testRedImageTarget redImage
            putStrLn "Drawing green!"
            writePng testGreenImageTarget greenImage
            putStrLn "Drawing blue!"
            writePng testBlueImageTarget blueImage
            putStrLn "Drawing white!"
            writePng testWhiteImageTarget whiteImage
            putStrLn "Finished!"
            -}