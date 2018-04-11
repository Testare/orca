module Orca.Reader.Layout.Grouping where

import Orca.Reader.Types

import Data.List(sortOn)

filterFunc :: Symbol -> Bool
filterFunc = do
    long <- filterFunc_long
    small <- filterFunc_small
    return (long && small)

filterFunc_long :: Symbol -> Bool
filterFunc_long = (\x-> maximum x < 5*(minimum x) ). (<*>) [fst,snd] . pure . symbolDims

filterFunc_small :: Symbol -> Bool
filterFunc_small = ((<) 10) . symbolWeight

-- Also filters out garbage
orderSymbolsToString :: Params -> [Symbol] -> [Symbol]
orderSymbolsToString params = concat . map (sortOn (fst . symbolOffset)) . stratify . filter filterFunc

orderSymbolsToStrings :: Params -> [Symbol] -> [[Symbol]]
orderSymbolsToStrings params = map (sortOn (fst . symbolOffset)) . stratify . filter filterFunc

stratify :: [Symbol] -> [[Symbol]]
stratify [] = []
stratify xs@(x:_) = thisLayer:(stratify nextLayers)
    where cutoff = (fst $ symbolDims x) + (snd $ symbolOffset x)
          (thisLayer, nextLayers) = span ((< cutoff) . snd . symbolOffset) xs 