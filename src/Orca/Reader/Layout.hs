module Orca.Reader.Layout where
import Graphics.Image(Image, VS, X, Bit, off, on)
import Graphics.Image.Interface(Vector, Pixel, fromVector, fromIx)
import qualified Data.Vector.Storable as V

type Coord = (Int, Int)
data Region = Region Int Int [Coord]

data Symbol = Symbol { x :: Int
                     , y :: Int
                     , height :: Int
                     , width :: Int
                     , img :: Image VS X Bit
                     }

gatherSymbols :: Image VS X Bit -> [Symbol]
gatherSymbols = coordsToSymbols . gatherCoords

gatherCoords :: Image VS X Bit -> [[Coord]]
gatherCoords _ = [[(0,0)]] -- Stub

parseRows :: Vector VS (Pixel X Bit) -> Int ->  Int -> ([[Coord]], [Region])
parseRows vec w y
    | V.null vec = ([],[])
    | otherwise = parseRow x y $ parseRows xs w (succ y)
    where (x,xs) = V.splitAt w vec 

parseRow :: Vector VS (Pixel X Bit) -> Int -> ([[Coord]], [Region]) -> ([[Coord]], [Region])
parseRow vec y rowParseData = ([], [])

parseBit :: ([[Coord]], [Region], [Region], Maybe Region) -> (Pixel X Bit, Coord) -> ([[Coord]], [Region], [Region], Maybe Region)
parseBit (residual, prevRegions, nextRegions, Nothing) (bit, coord)
    | bit == on =  ([], [], [], Nothing)
    | otherwise =  ([], [], [], Nothing)

parseBit (residual, prevRegions, nextRegions, (Just currentRegion)) (bit, coord)
    | bit == on =  ([], [], [], Nothing)
    | otherwise =  ([], [], [], Nothing)

regionOverlaps :: Region -> Coord -> Bool
regionOverlaps (Region rx rw _) (cx,_) = (rx <= cx) && (cx < (rx + rw))

regionPasses :: Region -> Coord -> Bool
regionPasses (Region rx rw _) (cx,_) = (succ cx) == (rx + rw)

coordsToSymbols :: [[Coord]] -> [Symbol]
coordsToSymbols [] = []
coordsToSymbols (coords:xs) = (Symbol x y h w img):(coordsToSymbols xs)
    where   (x,y,x2,y2) = Prelude.foldl (\(x1,y1,x2,y2) (i,j) -> ((min x1 i), (min y1 j), (max x2 i), (max y2 j))) (maxBound, maxBound, 0, 0) coords 
            (h,w) = (y2 - y, x2 - x)
            pointList = map ((\x -> (x, on)) . fromIx w) coords
            imgV = (V.replicate (h*w) off) V.// pointList
            img = fromVector (h,w) imgV


