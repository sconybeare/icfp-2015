{-# LANGUAGE ExistentialQuantification #-}

module GameState ( CellState       (..)
                 , GameSetup       (..)
                 , BoardDimensions (..)
                 , GameState       (..)
                 , PieceId         (..)
                 , Piece           (..)
                 , ACWRotation,    mkACWRot, fromACWRot
                 , BoardState,     mkBoard,  accessCell, lock
                 , Generator       (..)
                 ) where


--------------------------------------------------------------------------------
------------------------------------ Header ------------------------------------
--------------------------------------------------------------------------------


-- Imports
import qualified Data.Map.Strict  as M

import           Data.AffineSpace
import           Data.Map.Strict  (Map)

import           System.Random
import           Types            (BVect, Point (..))


--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------


-- | The state of a cell
data CellState = Empty -- ^ The cell is empty
               | Full  -- ^ The cell is full
               deriving (Eq, Enum, Show, Read)

-- | The dimensions of a board
data BoardDimensions = BDim { getWidth  :: Int
                            , getHeight :: Int
                            } deriving (Eq, Show, Read)

-- | A piece is a list of 'BVect's, which are all relative to the pivot.
newtype Piece = Piece [BVect]
              deriving (Eq, Show, Read)

-- | The state of a board, in terms of which cells are enabled or disabled
newtype BoardState = BState (Map Point CellState)
                   deriving (Eq, Show, Read)

-- | Static information for a game
data GameSetup = GSetup { getDimensions :: BoardDimensions
                        , getPieces     :: [Piece]
                        , getSeed       :: Int
                        , getLayout     :: BoardState
                        } deriving (Eq, Show, Read)

-- | Random generator existential type
data Generator = forall g. RandomGen g => Generator g

-- | Game state
data GameState = GState { getBoardState       :: BoardState
                        , getPieceCounter     :: Int
                        , getPieceId          :: PieceId
                        , getPieceLoc         :: Point
                        , getPieceOrientation :: ACWRotation
                        , getGenerator        :: Generator
                        }

-- | ID of a Piece
newtype PieceId = PieceId Int
                deriving (Eq, Show, Read)

-- | Anticlockwise rotation amount
newtype ACWRotation = ACWRot { fromACWRot :: Int
                             } deriving (Eq, Ord, Show, Read)


--------------------------------------------------------------------------------
---------------------------------- Instances -----------------------------------
--------------------------------------------------------------------------------


instance Random PieceId where
  random g = (PieceId $ i, g') where (i,g') = next g
  randomR _ = random

instance RandomGen Generator where
  next (Generator gen) = (x,Generator y) where (x,y) = next gen
  genRange (Generator gen) = genRange gen
  split (Generator gen) = (Generator x, Generator y) where (x,y) = split gen


--------------------------------------------------------------------------------
------------------------------- Public functions -------------------------------
--------------------------------------------------------------------------------


-- | Create a 'BoardState' from a 'Map' from 'Point's to 'CellState's
mkBoard :: Map Point CellState -> BoardState
mkBoard = BState

-- | Turn a number of anticlockwise turns into an 'ACWRotation'
mkACWRot :: Int -> ACWRotation
mkACWRot x = ACWRot $ x `mod` 6

-- | Access a cell at a point in a board with the given state and dimensions
accessCell :: BoardDimensions -> BoardState -> Point -> Maybe CellState
accessCell dim bst pt
  | insideBounds dim pt, BState cells <- bst = M.lookup pt cells
  | otherwise                                = Nothing

-- | "Lock" a piece into a board with the given state and dimensions
lock :: BoardDimensions -> BoardState -> Piece -> Point -> Maybe BoardState
lock dims bst@(BState stm) pc@(Piece vs) loc
  | not $ checkBounds     dims loc pc     = Nothing
  | not $ checkNotOverlap dims bst loc pc = Nothing
  | otherwise                             = Just $ BState $ insertPairs stm kvs
  where
    kvs = zip (map (loc .+^) vs) $ repeat Full
    insertPairs m []          = m
    insertPairs m ((k, v):ps) = insertPairs (M.insert k v m) ps


--------------------------------------------------------------------------------
------------------------------ Private functions -------------------------------
--------------------------------------------------------------------------------


-- | Check if the given 'Piece' overlaps any of the currently-full cells
checkNotOverlap :: BoardDimensions -> BoardState -> Point -> Piece -> Bool
checkNotOverlap gsu gst loc (Piece pcs) = all isEmpty cellStates
  where
    isEmpty x  = x == Just Empty
    cellStates = map (accessCell gsu gst) cells
    cells      = map (loc .+^) pcs

-- | Is a 'Piece' located at a 'Point' within the bounds of the board?
checkBounds :: BoardDimensions -> Point -> Piece -> Bool
checkBounds dims loc (Piece pcs) = all inside pcs
  where
    inside v = insideBounds dims (loc .+^ v)

-- | Is the given 'Point' within the given 'BoardDimensions'
insideBounds :: BoardDimensions -> Point -> Bool
insideBounds dims loc = not $ pointOutside (boardBounds dims) loc

-- | Is a 'point' outside of the region specified by a pair of 'Point's?
pointOutside :: (Point, Point) -> Point -> Bool
pointOutside (Point x1 y1, Point x2 y2) (Point c1 c2)
  | not (within x1 x2 c1) = True
  | not (within y1 y2 c2) = True
  | otherwise             = False
  where
    within mn mx val = (mn < val) && (val < mx)

-- | Get the upper left corner of the board with the given 'BoardDimensions'
pointMin :: BoardDimensions -> Point
pointMin _          = Point 0 0

-- | Get the lower right corner of the board with the given 'BoardDimensions'
pointMax :: BoardDimensions -> Point
pointMax (BDim w h) = Point w h

-- | Get the bounds of the board with the given 'BoardDimensions'
boardBounds :: BoardDimensions -> (Point, Point)
boardBounds dims = (pointMin dims, pointMax dims)
