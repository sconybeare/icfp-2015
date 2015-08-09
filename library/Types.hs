{-# LANGUAGE TypeFamilies #-}

-- | Defines types used generally in the program
module Types ( Trans (..)
             , Turn  (..)
             , Point (..)
             , BVect (..)
             ) where

--------------------------------------------------------------------------------
------------------------------------ Header ------------------------------------
--------------------------------------------------------------------------------

-- Imports
import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.VectorSpace


--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------


-- | A translation move
data Trans = E  -- ^ east
           | W  -- ^ west
           | SE -- ^ southeast
           | SW -- ^ southwest
           deriving (Eq, Show, Read)

-- | A turn move
data Turn = CW  -- ^ clockwise
          | ACW -- ^ anticlockwise
          deriving (Eq, Show, Read)

-- | A point in 2D space
--   We derive 'Ord' because 'Point's are used as keys in 'Data.Map's.
--   The ordering is lexicographic.
data Point = Point { getX :: Int
                   , getY :: Int
                   } deriving (Eq, Ord, Show, Read)

-- | An integer-valued vector with an east-southwest basis.
--   These represent directions on the board.
data BVect = BVect { getE  :: Int
                   , getSW :: Int
                   } deriving (Eq, Show, Read)

-- | A move can either be a translation or a rotation
data Move = Shift Trans -- ^ Translation
          | Rotate Turn -- ^ Rotation
          deriving (Eq, Show, Read)


--------------------------------------------------------------------------------
---------------------------------- Instances -----------------------------------
--------------------------------------------------------------------------------


-- | 'BVect's can be added and subtracted and have an identity (@BVect 0 0@).
instance AdditiveGroup BVect where
  zeroV = BVect 0 0
  BVect x1 y1 ^+^ BVect x2 y2 = BVect (x1+x2) (y1+y2)
  negateV (BVect x1 y1) = BVect (-x1) (-y1)

-- | 'BVect's can be scaled by an integer scalar.
instance VectorSpace BVect where
  type Scalar BVect = Int
  c *^ BVect x y = BVect (c*x) (c*y)

-- | 'Point's can be added with 'BVect's and can be subtracted.
instance AffineSpace Point where
  type Diff Point = BVect
  Point x y .+^ BVect e sw = Point x' y' where
    y' = y + sw
    x' = x + e - sw + case (odd y, odd y') of
                       (False, True) -> 0
                       _             -> 1
  Point x' y' .-. Point x y = BVect e sw where
    sw = y' - y
    e = x' - x + sw - case (odd y, odd y') of
                       (False, True) -> 0
                       _             -> 1
