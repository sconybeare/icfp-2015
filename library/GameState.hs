module GameState (CellState (..), BoardState) where

import Types (Point (..))
import GameSetup (BoardDimensions (..))

data CellState = Full | Empty
newtype BoardState = BState [[CellState]]

data Piece

accessCell :: BoardState -> Point -> CellState
accessCell _ _ = undefined

lock :: BoardDimensions -> BoardState -> Piece -> Point -> Maybe BoardState
lock _ _ _ _ = undefined
