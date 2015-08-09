module GameState ( CellState (..)
                 , BoardState
                 , accessCell
                 , lock
                 , ACWRotation
                 , fromACWRot
                 , acwRot
                 , GameSetup (..)
                 , BoardDimensions (..)
                 , GameState (..)
                 , PieceId (..)
                 , Piece (..)
                 ) where

import           Data.AffineSpace
import           Types            (Point (..), Vect)
import qualified Data.Map.Strict as M

data GameSetup = GameSetup { getDimensions :: BoardDimensions
                           , getPieces :: [Piece]
                           , getSeed :: Int
                           , getLayout :: BoardState
                           }
data BoardDimensions = BDim { getWidth :: Int, getHeight :: Int }

newtype Piece = Piece [Vect]

data CellState = Empty | Full
               deriving (Eq, Enum, Show, Read)

newtype BoardState = BState (M.Map Point CellState)
                   deriving (Show)

data GameState = GState { getBoardState :: BoardState
                        , getPieceCount :: Int
                        , getPieceId :: PieceId
                        , getPieceLoc :: Point
                        , getPieceOrientation :: ACWRotation
                        }

newtype PieceId = PieceId Int

newtype ACWRotation = ACWRot Int
acwRot :: Int -> ACWRotation
acwRot x = ACWRot $ x `mod` 6
fromACWRot :: Integral a => ACWRotation -> a
fromACWRot (ACWRot x) = fromIntegral x

accessCell :: BoardDimensions -> BoardState -> Point -> Maybe CellState
accessCell dim bst pt
  | insideBounds dim pt
  , BState cells <- bst
    = M.lookup pt cells
  | not $ insideBounds dim pt
    = Nothing

lock :: BoardDimensions -> BoardState -> Piece -> Point -> Maybe BoardState
lock dims bst@(BState m) pc@(Piece vs) loc
  | not $ checkBounds dims loc pc = Nothing
  | not $ checkOverlap dims bst loc pc = Nothing
  | True = Just $ BState $ insertPairs kvs m where
      kvs = zip (map (loc .+^) vs) $ repeat Full

insertPairs :: Ord k => [(k,a)] -> M.Map k a -> M.Map k a
insertPairs [] m = m
insertPairs ((key,val) : kvs) m = insertPairs kvs (M.insert key val m)

checkOverlap :: BoardDimensions -> BoardState -> Point -> Piece -> Bool
checkOverlap gsu gst loc (Piece pcs) = and bools where
  bools      = map (Just Empty ==) cellStates
  cellStates = map (accessCell gsu gst) cells
  cells      = map (loc .+^) pcs
  

checkBounds dims loc (Piece [])     = insideBounds dims loc
checkBounds dims loc (Piece (v:vs)) = insideBounds dims loc &&
                                      checkBounds dims (loc .+^ v) (Piece vs)

pointMin :: BoardDimensions -> Point
pointMin _          = Point 0 0

pointMax :: BoardDimensions -> Point
pointMax (BDim w h) = Point w h

insideBounds dims loc = not $ pointOutside (boardBounds dims) loc

boardBounds :: BoardDimensions -> (Point, Point)
boardBounds dims = (pointMin dims, pointMax dims)

pointOutside :: (Point, Point) -- ^ Rectangular region
             -> Point          -- ^ Point to determine if contained with area
             -> Bool           -- ^ Result
pointOutside (Point x1 y1, Point x2 y2) (Point c1 c2)
  | not (within x1 x2 c1) = True
  | not (within y1 y2 c2) = True
  | otherwise             = False
  where
    within mn mx val = (mn < val) && (val < mx)

