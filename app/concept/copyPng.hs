import Codec.Picture -- (readImage, PixelRGB8, DynamicImage, Image, pixelAt, generateImage, imageHeight, imageWidth, convertRGBA8)
--import Codec.Picture.Png (writeDynamicPng, writePng)
import Codec.Picture.Types(Pixel, PlaneRed, pixelFoldMap, pixelFold)
import Data.Monoid(Sum, getSum, (<>))
import Data.List
import Data.Int(Int64)

data Color = RED | GREEN | BLUE
data ColorReport = ColorReport Integer Integer Integer Integer deriving Show
instance Monoid ColorReport where
    mempty = ColorReport 0 0 0 0
    mappend (ColorReport a1 a2 a3 a4) (ColorReport b1 b2 b3 b4) = ColorReport (a1+b1) (a2+b2) (a3+b3) (a4+b4)

data KMeanList = KMeanList PixelRGB8 [PixelRGB8] deriving (Ord, Eq, Show)
data KMeansPixelReport = KMeansPixelReport [KMeanList] deriving (Show)

{-Test data-}
testImageSource = "./test_source4.png"
testImageTarget = "./test_target.png"
testBlackImageTarget = "./black_target.png"
testWhiteImageTarget = "./white_target.png"
testRedImageTarget = "./red_target.png"
testGreenImageTarget = "./green_target.png"
testBlueImageTarget = "./blue_target.png"

testLst = KMeanList (PixelRGB8 100 100 100) []
testLst1 = KMeanList (PixelRGB8 80 80 80) [(PixelRGB8 85 85 80)]
testPixel = PixelRGB8 81 81 81

pixelRotation :: (Pixel a) => Image a -> Int -> Int -> Int -> Int -> a
pixelRotation img width height x y = pixelAt img (mod (height - y) height) (mod x width)

processPng :: (Pixel b) => Image b -> Image b
processPng img = generateImage rotateFn width height 
    where
        width = imageHeight img
        height = imageWidth img
        rotateFn = pixelRotation img width height

reportOnColor :: PixelRGB8 -> ColorReport
reportOnColor = do
    red <- (countPixelComponent RED)
    green <- (countPixelComponent GREEN)
    blue <- (countPixelComponent BLUE)
    return $ ColorReport red green blue 1

countPixels :: (Integral b) => Image PixelRGB8 -> Sum b
countPixels = pixelFoldMap (pure . (countPixelComponent BLUE))

pixelReport :: Image PixelRGB8 -> ColorReport
pixelReport = pixelFoldMap reportOnColor

countPixelComponent :: (Integral b) => Color -> PixelRGB8 -> b
countPixelComponent RED (PixelRGB8 r g b) = fromIntegral r
countPixelComponent GREEN (PixelRGB8 r g b) = fromIntegral g
countPixelComponent BLUE (PixelRGB8 r g b) = fromIntegral b

{-Clustering-}
fromPixel :: PixelRGB8 -> (Int64, Int64, Int64)
fromPixel (PixelRGB8 r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)

toPixel :: (Integral a)=>(a, a, a) -> PixelRGB8
toPixel (r, g, b) = (PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b))

sumPixels :: (Integral a) => [(a, a, a)] -> (a, a, a)
sumPixels = foldl (\(r,g,b) (r1,g1,b1) -> (r+r1,g+g1,b+b1)) (0,0,0) 

averagePixel :: KMeanList -> PixelRGB8
averagePixel (KMeanList _ []) = (PixelRGB8 0 0 0)
averagePixel (KMeanList _ lst) = toPixel $ g $ sumPixels $ map fromPixel lst
    where f x = flip div (fromIntegral $ length lst) x
          g (x,y,z) = (f x, f y, f z)

invertPixel :: PixelRGB8 -> PixelRGB8
invertPixel (PixelRGB8 r g b) = (PixelRGB8 (255-r) (255-g) (255-b))

drawClusterImage :: Image PixelRGB8 -> KMeanList -> Image PixelRGB8
drawClusterImage img means@(KMeanList _ lst) = pixelMap choosePixel img 
    where background = (invertPixel . averagePixel) $ means
          choosePixel px = if elem px lst then px else background

kClusterPixels :: Int -> KMeansPixelReport -> Image PixelRGB8 -> KMeansPixelReport
kClusterPixels 0 baseReport _ = baseReport
kClusterPixels 1 baseReport img = clusterPixels baseReport img
kClusterPixels n baseReport img = clusterPixels newReport img
    where lastIteration@(KMeansPixelReport kMeanLists) = kClusterPixels (pred n) baseReport img
          newReport = KMeansPixelReport $ map (\kList -> KMeanList (averagePixel kList) []) kMeanLists

clusterPixels :: KMeansPixelReport -> Image PixelRGB8 -> KMeansPixelReport
clusterPixels baseReport = pixelFold (clusterPixel) baseReport

clusterPixel :: KMeansPixelReport -> Int -> Int -> PixelRGB8 -> KMeansPixelReport
clusterPixel report@(KMeansPixelReport kMeans) x y pixel = KMeansPixelReport myMeans
    where   distances = map (\(KMeanList px _) -> pixelDistance pixel px) kMeans
            minDistance = minimum distances
            elemIndex = elemIndices minDistance distances !! 0
            indexedMeans = zip [0..] kMeans
            myMeans = map (\(i,(KMeanList px lst)) -> (KMeanList px (if (i == elemIndex) then pixel:lst else lst))) indexedMeans

-- Test starting reports
binaryClusterReport :: KMeansPixelReport
binaryClusterReport = KMeansPixelReport [(KMeanList (PixelRGB8 255 255 255) []), (KMeanList (PixelRGB8 0 0 0) [])]
colorClusterReport = KMeansPixelReport [(KMeanList (PixelRGB8 255 0 0) []), (KMeanList (PixelRGB8 0 255 0) []), (KMeanList (PixelRGB8 0 0 255) []) ] 
colorClusterReport2 = KMeansPixelReport [(KMeanList (PixelRGB8 168 133 120) []), (KMeanList (PixelRGB8 28 43 37) []), (KMeanList (PixelRGB8 35 43 53) [])]

pixelDistance :: PixelRGB8 -> PixelRGB8 -> Int
pixelDistance = pixelDistance1

pixelDistance1 (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = (diff r1 r2) + (diff g1 g2) + (diff b1 b2)
        where diff a b = ((fromIntegral b) - (fromIntegral a))^2

pixelDistance2 (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = (diff r1 r2) + (diff g1 g2) + (diff b1 b2)
        where diff a b = (abs (fromIntegral b) - (fromIntegral a))

pixelDistanceByShade (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = diff (r1+g1+b1) (r2+g2+b2)
        where diff a b = ((fromIntegral b) - (fromIntegral a))^2

main :: IO ()
main = do
    image <- readImage testImageSource ::IO (Either String DynamicImage)-- Either String DynamicImage
    flip (either (putStrLn . (++ "Error: "))) (convertRGB8 <$> image) $ do
        report <- kClusterPixels 4 colorClusterReport
        binaryReport <- clusterPixels binaryClusterReport
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