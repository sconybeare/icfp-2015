module GameSetup where

data GameSetup = GameSetup { getDimensions :: BoardDimensions }
data BoardDimensions = BDim { getWidth :: Int, getHeight :: Int }

 -- data Piece = 
