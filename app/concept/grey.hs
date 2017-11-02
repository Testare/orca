import Codec.Picture
import Codec.Picture.Types
import Orca.Reader.Greyscale
import Orca.Reader.Processing
import Orca.Testing

writeHistogram :: FilePath -> [Int] -> IO ()
writeHistogram fp hsdata = writeFile fp d8ta
    where d8ta = concat $ flip (++) "\n" . show <$> hsdata

main :: IO ()
main = do
    let imgs = [testImageSource, testBigImageSource]
    putStrLn $ "Use big image? (0=small,1=big)"
    imgFile <- (readLn) >>= (pure . (imgs !!))
    putStrLn $ "Write stats?(y/n)"
    writeStatsBool <- getLine >>= return . (== "y")
    putStrLn $ "Would you like to apply a threshold? (y/n)"
    applyThreshIn <- (getLine :: IO [Char])
    imgModFn <- if applyThreshIn /= "y" then return id else do 
        putStrLn $ "Lower bound:"
        low <- readLn
        putStrLn $ "High bound:"
        high <- readLn
        putStrLn $ "CutOff or BinaryImage?"
        threshOp <- readLn
        return $ threshold threshOp low high

    tryWithImage imgFile $ \img -> do

        let greyImg = imgModFn $ convertToGreyscale img
        let hsdata = histogramToList $ createHistogram $ greyImg
        counter <- getCounter greyAttemptCounter >>= (return . show)
        incCounter greyAttemptCounter
        let outimg = (greyAttemptDirectory ++ counter ++ targetFile)
        putStrLn $ "Putting this where it belongs: " ++ outimg
        if writeStatsBool then do
            writePng outimg $ greyImg
            let outHs = (greyAttemptDirectory ++ counter ++ histogramFile)
            putStrLn $ "Writing histogram data: " ++ outHs
            writeHistogram outHs hsdata
            else return ()
