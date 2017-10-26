module Orca.Testing where
import Orca.Reader.Clustering
import Codec.Picture
import Codec.Picture.Types

testImageSource = "./test_source4.png"
testBigImageSource = "./test_source.JPG"
testImageTarget = "./test_target.png"
testImageTarget2 = "./test_target2.png"
testImageTarget3 = "./test_target3.png"
testBlackImageTarget = "./black_target.png"
testWhiteImageTarget = "./white_target.png"
testRedImageTarget = "./red_target.png"
testGreenImageTarget = "./green_target.png"
testBlueImageTarget = "./blue_target.png"
testGrayImageTarget = "./test_gray_target.png"

exampleMeanCluster = MeanCluster (PixelRGB8 255 0 127) mempty
exampleMeanCluster1 = MeanCluster (PixelRGB8 125 0 255) mempty
exampleClusters = [exampleMeanCluster, exampleMeanCluster1]
examplePixel = PixelRGB8 240 0 200