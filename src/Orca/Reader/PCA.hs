module Orca.Reader.PCA where 

import Orca.Reader.Types
--import Orca.Reader.Classification(correctDimensions)
--import Graphics.Image(Array, Image, VS, Pixel, toLists, resize, Bilinear(Bilinear), Border(Edge))
import Graphics.Image(Pixel(PixelY), X(..), Bit, Y, dims, resize, Bilinear(Bilinear), Border(Edge))
import Graphics.Image.Interface(toListPx, toDouble)
import qualified Graphics.Image as HIP
import Numeric.LinearAlgebra(tr, compactSVD, toColumns, fromColumns, Vector(..), Matrix(..), (><), (<.>), add, cmap, toLists, toList, fromList)
import qualified Numeric.LinearAlgebra as HMat
import Orca.Helper(display)

testImagesToMatrix :: IO ()
testImagesToMatrix = do
    let imgs = [ HIP.fromLists [[PixelY 1.0, PixelY 1.0, PixelY 0.0], [PixelY 0.0, PixelY 1.0, PixelY 0.0]]
               , HIP.fromLists [[PixelY 1.0, PixelY 0.0, PixelY 0.0], [PixelY 0.0, PixelY 1.0, PixelY 0.0]] 
               , HIP.fromLists [[PixelY 1.0, PixelY 1.0, PixelY 1.0], [PixelY 1.0, PixelY 0.0, PixelY 0.0]] 
               ]
        mg = imagesToMatrix' imgs
    putStrLn $ show $ mg
    putStrLn $ show $ toColumns mg
    putStrLn $ show $ getColumnMean mg
    putStrLn $ show $ averageColumns mg
    let (u,_,_) = compactSVD (averageColumns mg)
    putStrLn $ show $ u
    putStrLn $ show $ map (sum . (map (** 2)) . toList ) $ toColumns u
    let pv = projectVector' (head $ toColumns mg) (head $ toColumns u)
    putStrLn $ show $ pv
    putStrLn $ show $ projectVector'  
        (fromList [0.0, 1.0, 0.0, 0.0, 1.0, 1.0])
        (head $ toColumns u)
    display $ eigenFaceToImage ((2,3), snd pv)

{- public -}
manhattanDistance :: EigenFace -> Double
manhattanDistance = foldl1 (+) . map abs . toList . snd

{- public -}
euclideanDistance :: EigenFace -> Double
euclideanDistance = foldl1 (+) . map (**2) . toList . snd

{- public -}
eigenFaceToImage :: EigenFace -> GrayImage
eigenFaceToImage ((h,w), data') = HIP.fromLists $ groupIt $ map (PixelY . (/ 2) . (+ 1)) $ toList data'
    where groupIt [] = [] 
          groupIt lst = let (sect,rem) = splitAt w lst in sect:(groupIt rem)

eigenFaceSubtract :: EigenFace -> EigenFace -> EigenFace
eigenFaceSubtract (d, a) (_,b) = (d, fromList $ zipWith (-) (toList a) (toList b))

bitToGrayImage :: BitImage -> GrayImage
bitToGrayImage = HIP.fromLists . map (map bitToGray) . HIP.toLists --probably a method exists to map pixels on an image

bitImageToEigenFace :: GrayImage -> EigenFace
bitImageToEigenFace img = (d, fromList $ map (head . toListPx) $ concat $ HIP.toLists img)
    where d = dims img

{- public -}
generateEigenFaces :: [BitImage] -> [EigenFace]
generateEigenFaces = generateEigenFaces' placeholderDims

generateEigenFaces' :: (Int, Int) -> [BitImage] -> [EigenFace]
generateEigenFaces' d imgs = map (\x->(d,x)) $ toColumns u
    where grayInput = map (HIP.fromLists . map (map bitToGray) . HIP.toLists) imgs
          (u,_,_) = compactSVD $ averageColumns $ imagesToMatrix'' d grayInput -- Need to fix this to pass DIMS

{- public -}
projectOnFaces :: (Int, Int) -> BitImage -> Vector Double -> [EigenFace] -> EigenFace
projectOnFaces d bi meanVec efs = (d, efVector)
    where biVector = fromList $ flip (zipWith (-)) (toList meanVec) $ map (toDouble . head . toListPx) $ concat $ HIP.toLists $ correctDimensions d bi
          efVector = foldl1 add $ map (projectVector biVector . snd) efs

projectVector :: Vector Double -> Vector Double -> Vector Double
projectVector a = snd . projectVector' a
    -- It is important for this to work that b be a unit vector
projectVector' :: Vector Double -> Vector Double -> (Double, Vector Double)
projectVector' a b = (dp, cmap (*dp) b)
    where dp = a <.> b

meanFace :: (Int, Int) -> Matrix Double -> EigenFace
meanFace d m = (d, meanVector m)

meanVector :: Matrix Double -> Vector Double
meanVector m = cmap (/ (fromIntegral $ length vecs)) $ foldl1 add vecs
    where vecs = toColumns m

averageColumns :: Matrix Double -> Matrix Double
averageColumns m = fromColumns $ map fromList $ map (\x -> zipWith (-) x mean) columns
    where columns = map toList $ toColumns m
          n = fromIntegral $ length columns
          mean = map (/ n) $ foldl1 (zipWith (+)) columns

getColumnMean :: Matrix Double -> Vector Double
getColumnMean = do
    columns <- map toList . toColumns
    let n = fromIntegral $ length columns
    return $ fromList $ map (/ n) $ foldl1 (zipWith (+)) columns

correctDimensions :: HIP.Array vs cs e => (Int, Int) -> HIP.Image vs cs e -> HIP.Image vs cs e
correctDimensions = resize Bilinear Edge

placeholderDims :: (Int, Int)
placeholderDims = (50,50)

imagesToMatrix'' :: (Int, Int) -> [GrayImage] -> Matrix Double
imagesToMatrix'' d = imagesToMatrix' . map (correctDimensions d)

imagesToMatrix :: [GrayImage] -> Matrix Double
imagesToMatrix = imagesToMatrix'' placeholderDims

imagesToMatrix' :: [GrayImage] -> Matrix Double
imagesToMatrix' imgs = tr $ ((length imgs)><size) linearized -- Must be able to fix dimensions
        where vectorList = concat <$> HIP.toLists <$> imgs
              linearized = concat $ map (head . toListPx) <$> vectorList
              size = length $ head $ vectorList
        --HMat.fromLists? HMat.toLists?
        --HMat.tr may be necessary

--getNEigenVectors :: HMat.Matrix Double -> [HMat.]
--getNEigenVectors 
            --where (u,_,_) = compactSvd 

{- Helper functions -}

bitToGray :: HIP.Pixel HIP.X HIP.Bit -> HIP.Pixel HIP.Y Double 
bitToGray px = HIP.PixelY (if HIP.isOn px then 0.0 else 1.0)
