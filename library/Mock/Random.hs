-- | Generate random numbers for the mock server
module Mock.Random (LCGGen, mkLCGGen) where


--------------------------------------------------------------------------------
------------------------------------ Header ------------------------------------
--------------------------------------------------------------------------------


-- Imports
import           System.Random


--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------


data LCGGen = LCGGen [Int] deriving (Eq, Show, Read)

instance RandomGen LCGGen where
  next (LCGGen (x:xs)) = (x, LCGGen xs)
  genRange _           = (0, lcgModulus - 1)
  split (LCGGen k l)   = (LCGGen k (odds l), LCGGen (evens l))


--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------


-- | Generate a new linear congruential generator from the given seed
mkLCGGen :: Int -> LCGGen
mkLCGGen = LCGGen . randomList

-- | Infinite list of random numbers generated from the given seed
randomList :: Integral i => i -> [i]
randomList seed = r : randomList r
  where r = (seed * lcgMultiplier + lcgIncrement) `rem` lcgModulus


--------------------------------------------------------------------------------
---------------------------------- Constants -----------------------------------
--------------------------------------------------------------------------------


lcgModulus, lcgMultiplier, lcgIncrement :: Integral i => i
lcgModulus    = 2 ^ 32
lcgMultiplier = 1103515245
lcgIncrement  = 12345


--------------------------------------------------------------------------------
------------------------------ Utility functions -------------------------------
--------------------------------------------------------------------------------


odds, evens :: [a] -> [a]
odds  (a:_:xs) = a : odds  xs
evens (_:b:xs) = b : evens xs
