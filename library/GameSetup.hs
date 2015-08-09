module GameSetup where

import           Types

data GameSetup = GameSetup { getDimensions :: BoardDimensions }
data BoardDimensions = BDim { getWidth :: Int, getHeight :: Int }

newtype Piece = Piece [Vect]
