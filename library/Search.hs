{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns#-}
module Search where

import GameState
import Types (Trans (..), Turn (..), Vect (..), Point (..))
import Data.AffineSpace

data Move = Shift Trans | Rotate Turn

moveN :: Int -> Maybe Move
moveN n | 0 <= n
        , n < 6 = Just $ ((map Shift [E,SE,SW,W]) ++ (map Rotate [ACW,CW])) !! n
        | otherwise = Nothing


applyTurn :: Turn -> ACWRotation -> ACWRotation
applyTurn ACW (fromACWRot->x) = acwRot $ x+1
applyTurn ACW (fromACWRot->x) = acwRot $ x-1

applyRot :: ACWRotation -> Vect -> Vect
applyRot (fromACWRot->angle) v = (iterate rotACW v) !! angle where
  rotACW (Vect x y) = Vect y (y-x)

applyShift :: Trans -> Point -> Point
applyShift E  = (.+^ Vect 1 0)
applyShift SE = (.+^ Vect 1 1)
applyShift SW = (.+^ Vect 0 1)
applyShift W  = (.+^ Vect (-1) 0)

applyRotPiece :: ACWRotation -> Piece -> Piece
applyRotPiece rot (Piece vs) = Piece $ map (applyRot rot) vs

applyTurnPiece :: Turn -> Piece -> Piece
applyTurnPiece ACW = applyRotPiece (acwRot 1)
applyTurnPiece CW = applyRotPiece (acwRot (-1))

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
