
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
handler (EventKey (Char '1') Up _ _) g = g {gField = rotate0 (gField g)}
handler (EventKey (Char '2') Up _ _) g = g {gField = rotate1 (gField g)}
handler (EventKey (Char '3') Up _ _) g = g {gField = rotate2 (gField g)}
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

