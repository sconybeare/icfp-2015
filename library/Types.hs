{-# LANGUAGE TypeFamilies #-}

module Types where

import           Data.AdditiveGroup
import           Data.AffineSpace

data Move = E  -- ^ east
          | W  -- ^ west
          | SE -- ^ southeast
          | SW -- ^ southwest

data Turn = CW  -- ^ clockwise
          | ACW -- ^ anticlockwise

data Point = Point { getX :: Int, getY :: Int } deriving Show

data Vect = Vect { getE :: Int, getSW :: Int } deriving Show

instance AdditiveGroup Vect where
  zeroV = Vect 0 0
  Vect x1 y1 ^+^ Vect x2 y2 = Vect (x1+x2) (y1+y2)
  negateV (Vect x1 y1) = Vect (-x1) (-y1)

instance AffineSpace Point where
  type Diff Point = Vect
  Point x y .+^ Vect e sw = Point x' y' where
    y' = y + sw
    x' = x + e - sw + case (parity y, parity y') of
                       (Even, Odd) -> 0
                       _           -> 1
  Point x' y' .-. Point x y = Vect e sw where
    sw = y' - y
    e = x' - x + sw - case (parity y, parity y') of
                       (Even, Odd) -> 0
                       _           -> 1

parity :: Integral a => a -> Parity
parity n = if n `mod` 2 == 0 then Even else Odd

data Parity = Even | Odd
