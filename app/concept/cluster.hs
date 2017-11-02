import Orca.Reader.Clustering
import Orca.Reader.Processing
import Orca.Reader.Greyscale
import Orca.Testing
import System.Random
import Codec.Picture



main :: IO ()
main = tryWithImage testImageSource $ \img -> do
    rand <- getStdGen
    let threeClusters = {-(MeanCluster (PixelRGB8 81 83 104) mempty):-}(kRandomClusters 4 rand img)
    let iteratedClusters = clusterFold threeClusters img
    let oneIterations = runKIterations 7 threeClusters img
    let clusterImg = drawCluster img oneIterations
    let rainbowImg = pixelMap (rainbowMap oneIterations) clusterImg
    let rainbowMap = mapTo threeClusters
    putStrLn $ show rainbowMap

    --let iteratedClusters = pixelFold f threeClusters img
    putStrLn "Three clusters"
    putStrLn $ show threeClusters
    putStrLn "Iterated clusters"
    putStrLn $ show iteratedClusters
    putStrLn "Three iteration clusters"
    putStrLn $ show oneIterations
    putStrLn "Writing..."
    writePng testImageTarget $ clusterImg
    putStrLn "Writing rainbow.."
    writePng testImageTarget2 $ rainbowImg
    putStrLn "Completed!"


