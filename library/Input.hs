-- | Process command-line arguments
module Input (Input (..), getInput) where

--------------------------------------------------------------------------------
------------------------------------ Header ------------------------------------
--------------------------------------------------------------------------------


-- Imports
import qualified Data.Map         as Map

import           Data.Text        (Text)

import           Data.AffineSpace

import           CommandLine
import           GameState
import           Parse
import           Types
import           Utility


--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------


data Input = Input { getId          :: Int
                   , getGames       :: [GameSetup]
                   , getTimeLimit   :: Maybe Int
                   , getMemoryLimit :: Maybe Int
                   , getCores       :: Maybe Int
                   , getPhrases     :: [Text]
                   } deriving (Eq, Show, Read)


--------------------------------------------------------------------------------
------------------------------- Public functions -------------------------------
--------------------------------------------------------------------------------


getInput :: IO [Input]
getInput = getOptions >>= generateInput

generateInput :: Options -> IO [Input]
generateInput opts = do
  json <- processAllJSON (optFiles opts)
  return $ map (processInput opts) json


--------------------------------------------------------------------------------
------------------------------ Private functions -------------------------------
--------------------------------------------------------------------------------


processInput :: Options -> JSON -> Input
processInput opts json = Input (jsonId json)
                               (processSetup json)
                               (optTimeLimit opts)
                               (optTimeLimit opts)
                               (optCores     opts)
                               (optPhrases   opts)

processAllJSON :: [FilePath] -> IO [JSON]
processAllJSON = mapM $ checkValid <.> readJSON
  where
    checkValid (Just p) = p
    checkValid Nothing  = error "Invalid JSON in file"

processSetup :: JSON -> [GameSetup]
processSetup json = map setup (jsonSourceSeeds json)
  where
    dims   = processDimensions json
    pieces = processPieces     json
    layout = processLayout     json
    setup seed = GSetup dims pieces seed layout

processDimensions :: JSON -> BoardDimensions
processDimensions j = BDim (jsonWidth j) (jsonHeight j)

processPieces :: JSON -> [Piece]
processPieces j = map processPiece $ jsonPieces j

processLayout :: JSON -> BoardState
processLayout j = processBoard (jsonWidth j) (jsonHeight j) (jsonFilled j)

processPiece :: JSONPiece -> Piece
processPiece (JSONPiece mems pivot) = Piece $ map (.-. pivotPt) memPts
  where
    memPts  = map cellToPoint mems
    pivotPt = cellToPoint pivot

processBoard :: Int -> Int -> [JSONCell] -> BoardState
processBoard w h cells = mkBoard $ compose (map addCell cells) board
  where
    addCell    = addPoint . cellToPoint
    addPoint p = Map.insert p Full
    board      = Map.fromList points
    points     = concat [[(Point x y, Empty) | y <- [0 .. h]] | x <- [0 .. w]]

cellToPoint :: JSONCell -> Point
cellToPoint (JSONCell x y) = Point x y
