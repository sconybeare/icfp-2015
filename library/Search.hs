{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GADTs#-}

module Search where

import           Data.AffineSpace
import           GameState
import           Types            (BVect (..), Point (..), Trans (..),
                                   Turn (..))
import           System.Random

data Move = Shift Trans | Rotate Turn

moveN :: Int -> Maybe Move
moveN n | 0 <= n, n < 6 = Just $ (shift ++ rot) !! n
        | otherwise     = Nothing
  where
    shift = map Shift [E, SE, SW, W]
    rot   = map Rotate [ACW, CW]

applyTurn :: Turn -> ACWRotation -> ACWRotation
applyTurn ACW (fromACWRot->x) = mkACWRot $ x+1
applyTurn  CW (fromACWRot->x) = mkACWRot $ x-1

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
                  -> Move -> Maybe (BoardState, PieceUpdate)
applyMoveBoard dim bst pc pt rot mov =
  case (lockCurrMaybe, lockNextMaybe) of
   (Nothing, _) -> Nothing
   (Just lockCurr, Nothing) -> Just (lockCurr, NewPiece)
   (Just lockNext, Just _) -> Just (lockNext, SamePiece)
  where lockCurrMaybe = lockOri dim bst pc pt rot
        lockNextMaybe = lock dim bst newPc newPt
        newPc = applyMovePiece mov pc
        newPt = applyMovePoint mov pt

pieceSpawnLoc :: BoardDimensions -> Piece -> Point
pieceSpawnLoc (BDim bWidth _) (Piece vs) = Point x y
  where y = negate $ minimum $ map (getY . (Point 0 0 .+^)) vs
        x = startingPXMin - pxMin
        startingPXMin = (bWidth - pWidth) `div` 2
        pWidth = pxMax - pxMin
        pxMax = maximum pxs
        pxMin = minimum pxs
        pxs = map (getX . ((Point 0 y) .+^)) vs
        

applyMoveGameState :: GameSetup -> GameState -> Move -> Maybe GameState
applyMoveGameState gsu gst mov = 
  case applyMoveBoard dim bst pc loc ang mov of
   Just (newBoard, NewPiece) -> Just $ GState newBoard (ct+1) (PieceId newPieceId) spawnLoc (mkACWRot 0) gen'
   Just (newBoard, SamePiece) -> Just $ GState newBoard ct pcid newLoc newAng gen'
   Nothing -> Nothing
  where
    GSetup dim pcs _ _ = gsu
--    GState bst ct pcid loc ang gen = gst
    bst = getBoardState gst
    ct  = getPieceCounter gst
    pcid= getPieceId gst
    loc = getPieceLoc gst
    ang = getPieceOrientation gst
    gen = getGenerator gst
    (newPieceId, gen') = random gen
    spawnLoc = pieceSpawnLoc dim newPiece
    pc = pcs !! (\(PieceId x) -> x) pcid
    newPiece = pcs !! newPieceId
    newLoc = applyMovePoint mov loc
    newAng = applyMoveAngle mov ang

approxScore :: GameSetup -> GameState -> Int
approxScore gsu gst = undefined
