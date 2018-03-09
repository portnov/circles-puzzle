
module Main where

import Control.Monad (forM_, msum)
import Text.Printf
import Data.List (findIndex)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState

import Debug.Trace

import Types
import Circles

data Game = Game {
    gField :: Field,
    gSelectedCycle :: Maybe Int,
    gCurrentTurn :: Maybe Turn,
    gTime :: Float,
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

selectTurn :: Bool -> Game -> Game
selectTurn cw g =
  case gSelectedCycle g of
    Nothing -> g
    Just idx -> setTurn cw idx g

setTurn :: Bool -> Int -> Game -> Game
setTurn cw idx g =
  case gCurrentTurn g of
    Nothing -> g {gCurrentTurn = Just (Turn cw idx)}
    Just _ -> g

handler :: Event -> Game -> Game
handler (EventKey (Char 'd') Up (Modifiers {shift=Up}) _) g = setTurn True 0 g
handler (EventKey (Char 'w') Up (Modifiers {shift=Up}) _) g = setTurn True 1 g
handler (EventKey (Char 'a') Up (Modifiers {shift=Up}) _) g = setTurn True 2 g
handler (EventKey (Char 'D') Up (Modifiers {shift=Down}) _) g = setTurn False 0 g
handler (EventKey (Char 'W') Up (Modifiers {shift=Down}) _) g = setTurn False 1 g
handler (EventKey (Char 'A') Up (Modifiers {shift=Down}) _) g = setTurn False 2 g
handler (EventKey (MouseButton LeftButton) Up _ _) g = selectTurn False g
handler (EventKey (MouseButton RightButton) Up _ _) g = selectTurn True g
handler (EventMotion p) g = g {gSelectedCycle = selectCycle p'}
  where
    viewPort = viewStateViewPort (gView g)
    p' = invertViewPort viewPort p
handler evt g = g {gView = updateViewStateWithEvent evt (gView g)}

secondsPerTurn :: Float
secondsPerTurn = 1.0

animation :: Float -> Game -> Game
animation dt g =
  case gCurrentTurn g of
    Nothing -> g 
    Just turn ->
      let newTime = gTime g + dt
      in  if newTime >= secondsPerTurn
            then g {gTime = 0.0, gCurrentTurn = Nothing, gField = makeTurn turn (gField g)}
            else g {gTime = newTime}

drawOverlay :: Game -> Picture
drawOverlay g =
  case gSelectedCycle g of
    Nothing -> blank
    Just idx -> 
      let (x0,y0) = cycleCenters !! idx
      in  translate x0 y0 $ color black $ thickCircle radius thickness

drawAnimatedField :: Maybe Turn -> Float -> Field -> Picture
drawAnimatedField Nothing _ f = drawField f
drawAnimatedField (Just (Turn cw rotatedCycleIdx)) time (Field cycles) =
    pictures $ zipWith draw [0..] cycles
  where
    draw idx cycle =
      let center = cycleCenters !! idx
          picture = uncurry translate center $ drawCycle (check idx) cycle
          sign = if cw then 1 else -1
          angle = fromIntegral sign * (time / secondsPerTurn) * 60
      in  if idx == rotatedCycleIdx
             then rotateAround center angle picture
             else picture

    check idx co =
      let idx' = (idx - rotatedCycleIdx) `mod` 3
          fc = FieldCoordinate idx co

          match (fc2, FieldCoordinate fci _) = fc2 == fc && fci == rotatedCycleIdx

      in  case idx' of
            0 -> True
            1 -> fc `notElem` map fst allEquations
            2 -> not $ any match allEquations

render :: Game -> Picture
render g =
  let viewPort = viewStateViewPort (gView g)
      baseField = drawAnimatedField (gCurrentTurn g) (gTime g) (gField g)
      overlay = drawOverlay g
      timer = translate 200 0 $ scale 0.1 0.1 $ text $ printf "%0.4f" (gTime g)
      -- timer = translate 200 0 $ scale 0.1 0.1 $ text $ show (gCurrentTurn g)
  in  applyViewPortToPicture viewPort $ pictures [baseField, overlay, timer]

main :: IO ()
main = do
  let mode = InWindow "Circles" (800, 600) (0,0)
  let game = Game initialField Nothing Nothing 0 viewStateInit
  print initialField
  play mode white 10 game render handler animation

