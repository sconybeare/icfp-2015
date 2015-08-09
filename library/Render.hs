module Render where

import           Control.Monad

import           Data.Complex
import           Data.Matrix                  (Matrix)
import qualified Data.Matrix                  as M
import           Data.Vector                  (Vector, (!), (//))
import qualified Data.Vector                  as V

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Timer

type GridElem = UI ()
type Grid = Matrix GridElem

render :: (Grid -> Grid) -> IO ()
render xformer = startGUI defaultConfig { jsStatic = Nothing } (setup xformer)

canvasSize, clockInterval :: Num n => n
canvasSize = 2000
clockInterval = 50

constMatrix :: Int -> Int -> a -> Matrix a
constMatrix x y val = M.matrix x y (const val)

(///) :: Matrix a -> [(Int, Int, a)] -> Matrix a
m /// []             = m
m /// ((a, b, c):xs) = M.setElem c (a, b) m /// xs

setup :: (Grid -> Grid) -> Window -> UI ()
setup stateTransformer window = do
    _ <- return window # set title "Canvas - Examples"

    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width  canvasSize
        # set style [("border", "solid black 1px"), ("background", "#eee")]

    clock <- timer # set interval clockInterval
    start clock

    _ <- getBody window #+ [ column [element canvas] ]

    canvas # UI.clearCanvas
    set UI.fillStyle (UI.htmlColor "#23364A") (return canvas)
    UI.fillRect (0, 0) canvasSize canvasSize canvas
    let fillProp color = do
          set UI.fillStyle (UI.htmlColor color) (return canvas)
          UI.fill canvas
          return ()
    let defaultProp = fillProp "#A2C5E8"
    let darkProp    = fillProp "#D2691E"
    let gridSize    = 8
    let defaultData = constMatrix (2 * gridSize) gridSize defaultProp
    -- example of modifying defaultData:
    -- let customData  = (defaultData /// [ (5, 0, darkProp)
    --                                    , (5, 3, darkProp)
    --                                    , (5, 5, darkProp)
    --                                    , (5, 7, darkProp)
    --                                    ])

    clockEv <- accumE defaultData $ stateTransformer <$ UI.tick clock
    onEvent clockEv $ genGrid canvas

genGrid :: UI.Canvas
        -> Matrix GridElem
        -> UI ()
genGrid canvas aGrid =
  forM_ [1, 3 .. (M.nrows aGrid)] $ \n -> do
    genRow canvas (s1 n) (M.getRow n       aGrid) sc
    genRow canvas (s2 n) (M.getRow (n + 1) aGrid) sc
  where
    sc          = 50
    psc         = 10 + sc
    s1 n        = (1.00 * psc, (fromIntegral n)     * psc)
    s2 n        = (2.25 * psc, (fromIntegral n + 1) * psc)

genRow :: UI.Canvas
       -> (Double, Double)
       -> Vector GridElem
       -> Double
       -> UI ()
genRow canvas (startX, startY) aRow scale =
  forM_ [0 .. V.length aRow - 1] $ \n ->
    genHex canvas (startX + offset n, startY) (aRow ! n) scale
  where
    offset n = 3 * scale * fromIntegral n

genHex :: UI.Canvas
       -> (Double, Double)
       -> GridElem
       -> Double
       -> UI ()
genHex canvas (startX, startY) props scale = do
  canvas # UI.beginPath
  movePos $ vertex 0
  forM_ [1 .. 6] $ linePos . vertex
  canvas # UI.closePath
  props
  canvas # UI.stroke
  where
    fromComp (x :+ y) = (x, y)
    startPos          = startX :+ startY
    sixthROU          = cis $ pi / 3

    vertex :: Integer -> Complex Double
    vertex n = ((sixthROU ^ n) * (scale :+ 0)) + startPos

    movePos pos = flip UI.moveTo canvas $ fromComp pos
    linePos pos = flip UI.lineTo canvas $ fromComp pos
