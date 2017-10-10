import Codec.Picture -- (readImage, PixelRGB8, DynamicImage, Image, pixelAt, generateImage, imageHeight, imageWidth, convertRGBA8)
--import Codec.Picture.Png (writeDynamicPng, writePng)
import Codec.Picture.Types(Pixel, PlaneRed, pixelFoldMap, pixelFold)
import Data.Monoid(Sum, getSum, (<>))
import Data.List

data Color = RED | GREEN | BLUE
data ColorReport = ColorReport Integer Integer Integer Integer deriving Show
instance Monoid ColorReport where
    mempty = ColorReport 0 0 0 0
    mappend (ColorReport a1 a2 a3 a4) (ColorReport b1 b2 b3 b4) = ColorReport (a1+b1) (a2+b2) (a3+b3) (a4+b4)

data KMeanList = KMeanList PixelRGB8 [PixelRGB8] deriving (Ord, Eq, Show)
data KMeansPixelReport = KMeansPixelReport [KMeanList] deriving (Show)

{-Test data-}
testImageSource = "./test_source.png"
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


drawClusterImage :: Image PixelRGB8 -> KMeanList -> Image PixelRGB8
drawClusterImage img means@(KMeanList _ lst) = pixelMap choosePixel img 
    where background = PixelRGB8 255 255 255
          choosePixel px = if elem px lst then px else background

clusterPixels :: KMeansPixelReport -> Image PixelRGB8 -> KMeansPixelReport
clusterPixels baseReport = pixelFold (clusterPixel) baseReport

clusterPixel :: KMeansPixelReport -> Int -> Int -> PixelRGB8 -> KMeansPixelReport
clusterPixel report@(KMeansPixelReport kMeans) x y pixel = KMeansPixelReport myMeans
    where   distances = map (\(KMeanList px _) -> pixelDistance pixel px) kMeans
            minDistance = minimum distances
            elemIndex = elemIndices minDistance distances !! 0
            indexedMeans = zip [0..] kMeans
            myMeans = map (\(i,(KMeanList px lst)) -> (KMeanList px (if (i == elemIndex) then pixel:lst else lst))) indexedMeans

binaryClusterReport :: KMeansPixelReport
binaryClusterReport = KMeansPixelReport [(KMeanList (PixelRGB8 255 255 255) []), (KMeanList (PixelRGB8 0 0 0) [])]

colorClusterReport = KMeansPixelReport [(KMeanList (PixelRGB8 255 0 0) []), (KMeanList (PixelRGB8 0 255 0) []), (KMeanList (PixelRGB8 0 0 255) []) ] 

pixelDistance :: PixelRGB8 -> PixelRGB8 -> Int
pixelDistance (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = (diff r1 r2) + (diff g1 g2) + (diff b1 b2)
        where diff a b = ((fromIntegral b) - (fromIntegral a))^2

main2 :: IO ()
main2 = do
    image <- readImage testImageSource -- Either String DynamicImage
    let processedImage = processPng <$> convertRGB8 <$> image
    putStrLn $ either id (show . pixelReport) processedImage
    --let clusterReport =  clusterPixels binaryClusterReport <$> processedImage
    --let (KMeansPixelReport kMeans) = clusterReport
    let writeAction = (writePng testImageTarget) <$> processedImage
    --let writeClusterImage = (\x -> (writePng testBlackImageTarget) $ drawClusterImage x (kMeans !! 1)) <$> processedImage
    either (putStrLn . (\x -> "Error: " ++ x)) id writeAction
    --either (putStrLn . (\x -> "Error: " ++ x)) id writeClusterImage

main :: IO ()
main = do
    image <- readImage testImageSource ::IO (Either String DynamicImage)-- Either String DynamicImage
    flip (either (putStrLn . (++ "Error: "))) (convertRGB8 <$> image) $ do
        report <- clusterPixels colorClusterReport
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
            putStrLn "Drawing white!"
            writePng testWhiteImageTarget whiteImage
            putStrLn "Drawing red!"
            writePng testRedImageTarget redImage
            putStrLn "Drawing green!"
            writePng testGreenImageTarget greenImage
            putStrLn "Drawing blue!"
            writePng testBlueImageTarget blueImage
            putStrLn "Finished!"