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


odds, evens :: [a] -> [a]
odds  (a:_:xs) = a : odds  xs
evens (_:b:xs) = b : evens xs
