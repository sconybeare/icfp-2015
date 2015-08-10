{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Generate random numbers
module Random (LCGGen, mkLCGGen, randomList) where

--------------------------------------------------------------------------------
------------------------------------ Header ------------------------------------
--------------------------------------------------------------------------------


-- Imports
import           System.Random


--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------


newtype LCGGen = LCGGen [Int] deriving (Eq)

instance RandomGen LCGGen where
  next (LCGGen (x:xs)) = (x, LCGGen xs)
  genRange _           = (0, lcgModulus - 1)
  split (LCGGen l)     = (LCGGen (odds l), LCGGen (evens l))


--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------


-- | Generate a new linear congruential generator from the given seed
mkLCGGen :: Int -> LCGGen
mkLCGGen = LCGGen . randomList

-- | Infinite list of random numbers generated from the given seed
randomList :: Int -> [Int]
randomList seed = r : randomList r
  where r = (seed * lcgMultiplier + lcgIncrement) `rem` lcgModulus


--------------------------------------------------------------------------------
---------------------------------- Constants -----------------------------------
--------------------------------------------------------------------------------


lcgModulus, lcgMultiplier, lcgIncrement :: Int
lcgModulus    = 2 ^ 32
lcgMultiplier = 1103515245
lcgIncrement  = 12345


--------------------------------------------------------------------------------
------------------------------ Utility functions -------------------------------
--------------------------------------------------------------------------------


-- | All the odd-numbered elements of a (potentially infinite) list
odds :: [a] -> [a]
odds []       = []
odds [a]      = [a]
odds (a:_:xs) = a : odds xs

-- | All the even-numbered elements of a (potentially infinite) list
evens :: [a] -> [a]
evens []     = []
evens (_:xs) = evens xs
