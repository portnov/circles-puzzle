
module Main where

import Control.Monad (forM_, msum)
import Text.Printf
import Data.List (findIndex)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState

import Types
import Circles

data Game = Game {
    gField :: Field,
    gSelectedCycle :: Maybe Int,
    gView :: ViewState
  }

selectCycle :: Point -> Maybe Int
selectCycle (x,y) =
    findIndex (==True) [check center0 (-2*pi/3) (pi/3), check center1 0 pi, check center2 (2*pi/3) (-pi/3)]
  where
    check center@(x0,y0) alpha0 alpha1
      | alpha0 > alpha1 = check center alpha0 pi || check center (-pi) alpha1 
      | otherwise = let dx = x-x0
                        dy = y-y0
                        r = sqrt (dx*dx + dy*dy)
                        alpha = atan2 dy dx
                    in  (r < radius) && (alpha > alpha0) && (alpha < alpha1)

handler :: Event -> Game -> Game
handler (EventKey (Char 'd') Up (Modifiers {shift=Up}) _) g = g {gField = rotateCw 0 (gField g)}
handler (EventKey (Char 'w') Up (Modifiers {shift=Up}) _) g = g {gField = rotateCw 1 (gField g)}
handler (EventKey (Char 'a') Up (Modifiers {shift=Up}) _) g = g {gField = rotateCw 2 (gField g)}
handler (EventKey (Char 'D') Up (Modifiers {shift=Down}) _) g = g {gField = rotateCcw 0 (gField g)}
handler (EventKey (Char 'W') Up (Modifiers {shift=Down}) _) g = g {gField = rotateCcw 1 (gField g)}
handler (EventKey (Char 'A') Up (Modifiers {shift=Down}) _) g = g {gField = rotateCcw 2 (gField g)}
handler (EventKey (MouseButton LeftButton) Up _ _) g = g {gField = rotate (gField g)}
  where
    rotate f = case gSelectedCycle g of
                 Nothing -> f
                 Just idx -> rotateCcw idx f
handler (EventKey (MouseButton RightButton) Up _ _) g = g {gField = rotate (gField g)}
  where
    rotate f = case gSelectedCycle g of
                 Nothing -> f
                 Just idx -> rotateCw idx f
handler (EventMotion p) g = g {gSelectedCycle = selectCycle p'}
  where
    viewPort = viewStateViewPort (gView g)
    p' = invertViewPort viewPort p
handler evt g = g {gView = updateViewStateWithEvent evt (gView g)}

animation :: Float -> Game -> Game
animation _ = id

drawOverlay :: Game -> Picture
drawOverlay g =
  case gSelectedCycle g of
    Nothing -> blank
    Just idx -> 
      let (x0,y0) = cycleCenters !! idx
      in  translate x0 y0 $ color black $ thickCircle radius thickness

render :: Game -> Picture
render g =
  let viewPort = viewStateViewPort (gView g)
      baseField = drawField (gField g)
      overlay = drawOverlay g
  in  applyViewPortToPicture viewPort $ pictures [baseField, overlay]

main :: IO ()
main = do
  let mode = InWindow "Circles" (800, 600) (0,0)
  let game = Game initialField Nothing viewStateInit
  print initialField
  play mode white 100 game render handler animation

