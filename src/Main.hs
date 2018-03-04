
module Main where

import Control.Monad (forM_)
import Text.Printf
import Graphics.Gloss

import Types
import Circles

main :: IO ()
main = do
  let mode = InWindow "Circles" (800, 600) (0,0)
  let Field [c0, c1, c2] = initialField
  -- display mode white $ drawCycle [] c1
  let field = rotate2 initialField
  print initialField
  display mode white $ drawField field 
  -- display mode white $ drawField initialField

--   forM_ allEquations $ \(c1,c2) ->
--     printf "%s = %s\n" (show c1) (show c2)

