
module Main where

import Control.Monad (forM_)
import Text.Printf
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState

import Types
import Circles

data Game = Game {
    gField :: Field,
    gView :: ViewState
  }

handler :: Event -> Game -> Game
handler (EventKey (Char 'd') Up (Modifiers {shift=Up}) _) g = g {gField = rotate0cw (gField g)}
handler (EventKey (Char 'w') Up (Modifiers {shift=Up}) _) g = g {gField = rotate1cw (gField g)}
handler (EventKey (Char 'a') Up (Modifiers {shift=Up}) _) g = g {gField = rotate2cw (gField g)}
handler (EventKey (Char 'D') Up (Modifiers {shift=Down}) _) g = g {gField = rotate0ccw (gField g)}
handler (EventKey (Char 'W') Up (Modifiers {shift=Down}) _) g = g {gField = rotate1ccw (gField g)}
handler (EventKey (Char 'A') Up (Modifiers {shift=Down}) _) g = g {gField = rotate2ccw (gField g)}
handler evt g = g {gView = updateViewStateWithEvent evt (gView g)}

animation :: Float -> Game -> Game
animation _ = id

render :: Game -> Picture
render g =
  let viewPort = viewStateViewPort (gView g)
  in  applyViewPortToPicture viewPort $ drawField (gField g)

main :: IO ()
main = do
  let mode = InWindow "Circles" (800, 600) (0,0)
  let game = Game initialField viewStateInit
  print initialField
  play mode white 100 game render handler animation

