{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}

module Search where

import           Data.AffineSpace
import           GameState
import           Types            (BVect (..), Point (..), Trans (..),
                                   Turn (..))

data Move = Shift Trans | Rotate Turn

moveN :: Int -> Maybe Move
moveN n | 0 <= n, n < 6 = Just $ (shift ++ rot) !! n
        | otherwise     = Nothing
  where
    shift = map Shift [E, SE, SW, W]
    rot   = map Rotate [ACW, CW]

applyTurn :: Turn -> ACWRotation -> ACWRotation
applyTurn ACW (fromACWRot->x) = mkACWRot $ x+1
applyTurn ACW (fromACWRot->x) = mkACWRot $ x-1

applyRot :: ACWRotation -> BVect -> BVect
applyRot (fromACWRot->angle) v = (iterate rotACW v) !! angle where
  rotACW (BVect x y) = BVect y (y-x)

applyShift :: Trans -> Point -> Point
applyShift E  = (.+^ BVect 1 0)
applyShift SE = (.+^ BVect 1 1)
applyShift SW = (.+^ BVect 0 1)
applyShift W  = (.+^ BVect (-1) 0)

applyRotPiece :: ACWRotation -> Piece -> Piece
applyRotPiece rot (Piece vs) = Piece $ map (applyRot rot) vs

applyTurnPiece :: Turn -> Piece -> Piece
applyTurnPiece ACW = applyRotPiece (mkACWRot 1)
applyTurnPiece CW = applyRotPiece (mkACWRot (-1))

applyMovePiece :: Move -> Piece -> Piece
applyMovePiece (Rotate r) = applyTurnPiece r
applyMovePiece (Shift _) = id

lockOri :: BoardDimensions -> BoardState -> Piece -> Point -> ACWRotation
           -> Maybe BoardState
lockOri dim bst pc pt rot = lock dim bst (applyRotPiece rot pc) pt

applyMovePoint :: Move -> Point -> Point
applyMovePoint (Shift s) = applyShift s
applyMovePoint (Rotate _) = id

applyMoveAngle :: Move -> ACWRotation -> ACWRotation
applyMoveAngle (Shift _) = id
applyMoveAngle (Rotate r) = applyTurn r

data PieceUpdate = SamePiece | NewPiece

applyMoveBoard :: BoardDimensions -> BoardState -> Piece -> Point -> ACWRotation
                  -> Move -> Maybe (PieceUpdate, BoardState)
applyMoveBoard dim bst pc pt rot mov =
  case (lockCurr, lockNext) of
   (Nothing, _) -> Nothing
   (Just _, Nothing) -> lockCurr >>= Just . curry id SamePiece
   (Just _, Just _) -> lockNext >>= Just . curry id NewPiece
  where lockCurr = lockOri dim bst pc pt rot
        lockNext = lock dim bst newPc newPt
        newPc = applyMovePiece mov pc
        newPt = applyMovePoint mov pt
