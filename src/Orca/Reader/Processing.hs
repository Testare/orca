module Orca.Reader.Processing where

import Codec.Picture as JP
import Codec.Picture.Types as JPT
import Graphics.Image.IO as HipIO
import Graphics.Image.Interface as HipI
import Graphics.Image.Interface.Vector
import Graphics.Image.ColorSpace

tryWithImage :: String -> (JP.Image JPT.PixelRGB8 -> IO ()) -> IO ()
tryWithImage filepath f = do
    image <- JP.readImage filepath
    either (putStrLn . (mappend "Error with file: ")) f (convertRGB8 <$> image)

--betterTryWithPNG :: Readable (HipI.Image VS RGB Double) PNG => FilePath

tryWithPNG :: Readable img PNG => String -> (img -> IO ()) -> IO ()
tryWithPNG = flip tryWithImage2 PNG

tryWithJPG :: Readable img JPG => String -> (img -> IO ()) -> IO ()
tryWithJPG = flip tryWithImage2 JPG

tryWithImage2 :: Readable img format => String -> format -> (img -> IO ()) -> IO ()
tryWithImage2 filepath format f = do
    image <- HipIO.readImageExact format filepath
    either (putStrLn . (mappend "Error with file: ")) f image
