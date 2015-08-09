module Hash where

import Data.Bits (Bits, xor)
import System.Random
import GameState
import Data.Map.Lazy as Map (Map, fromList, lookup)
import Types (Point (..))
import Data.Monoid (mconcat)
import Data.Set (empty, member, insert)

generateHashes :: (RandomGen g, Random b, Bounded b, Ord b) =>
                  g -> [b]
generateHashes = filterHashes . randomRs (minBound, maxBound)

filterHashes :: Ord b => [b] -> [b]
filterHashes l = helpFilter empty l where
  helpFilter _ [] = []
  helpFilter s (x:xs) = if member x s
                        then helpFilter s xs
                        else x : helpFilter (insert x s) xs

data StateElement = FilledCell Point
                  | PiecePos Point
                  | PieceRot CWRotation
                  | NumPieces Int

generatorKey :: BoardDimensions -> StateElement -> Int
generatorKey (BDim w h) (FilledCell (Point x y)) = x+w*y `mod` w*h
generatorKey (BDim w h) (PiecePos (Point x y)) = gk0 + (x+w*y `mod` w*h) where
  gk0 = w*h
generatorKey (BDim w h) (PieceRot r) = gk0 + fromCWRot r where
  gk0 = 2*w*h
generatorKey (BDim w h) (NumPieces x) = gk0 + x where
  gk0 = 2*w*h + 6

elems :: BoardDimensions -> GameState -> [StateElement]
elems (BDim w h) gs = mconcat [fc, pp, pr, np] where
  fc = map FilledCell [Point x y | y <- [0..(h-1)], x <- [0..(w-1)]]
  pp = [PiecePos $ getPieceLoc gs]
  pr = [PieceRot $ getPieceOrientation gs]
  np = [NumPieces $ getPieceCount gs]

populateHashMap :: (RandomGen g, Random b, Bounded b, Ord b) =>
                   g -> Map Int b
populateHashMap gen = fromList $ zip [0..] $ generateHashes gen

hashState :: Bits b => BoardDimensions -> Map Int b -> GameState -> Maybe b
hashState bd m gs = sequence generators >>= (Just . foldl1 xor) where
  generators = map (flip Map.lookup m) genKeys
  genKeys = map (generatorKey bd) $ elems bd gs
