{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Hash where

import           Data.Bits     (Bits, xor)
import           Data.Map.Lazy as Map (Map, fromList, lookup)
import           Data.Set      (empty, insert, member)
import           GameState
import           System.Random
import           Types         (Point (..))

type RandomKey a = (Random a, Bounded a, Ord a)

generateHashes :: (RandomGen g, RandomKey b) => g -> [b]
generateHashes = filterHashes . randomRs (minBound, maxBound)

filterHashes :: Ord b => [b] -> [b]
filterHashes = helpFilter empty
  where
    helpFilter _ []     = []
    helpFilter s (x:xs) = if member x s
                          then helpFilter s xs
                          else x : helpFilter (insert x s) xs

data StateElement = SFilledCell Point
                  | SPiecePos Point
                  | SPieceRot ACWRotation
                  | SNumPieces Int
                  deriving (Eq, Ord, Show, Read)

pattern SFilled x y = SFilledCell (Point x y)
pattern SFloat  x y = SPiecePos   (Point x y)

generatorKey :: BoardDimensions -> StateElement -> Int
generatorKey (BDim w h) = generatorKey' w h

generatorKey' :: Int -> Int -> StateElement -> Int
generatorKey' w h (SFilled x y)  =                x + (w * y) `mod` (w * h)
generatorKey' w h (SFloat  x y)  = (w * h)     + (x + (w * y) `mod` (w * h))
generatorKey' w h (SPieceRot r)  = (2 * w * h) + fromACWRot r
generatorKey' w h (SNumPieces x) = (2 * w * h) + 6 + x

elems :: BoardDimensions -> GameState -> [StateElement]
elems (BDim w h) gs = pp : pr : np : fc
  where
    fc = map SFilledCell [Point x y | y <- [0 .. h - 1], x <- [0 .. w - 1]]
    pp = SPiecePos  $ getPieceLoc gs
    pr = SPieceRot  $ getPieceOrientation gs
    np = SNumPieces $ getPieceCounter gs

populateHashMap :: (RandomGen g, RandomKey b) => g -> Map Int b
populateHashMap gen = fromList $ zip [0..] $ generateHashes gen

hashState :: Bits b => BoardDimensions -> Map Int b -> GameState -> Maybe b
hashState bd m gs = sequence generators >>= (Just . foldl1 xor)
  where
    generators = map (`Map.lookup` m) genKeys
    genKeys = map (generatorKey bd) $ elems bd gs
