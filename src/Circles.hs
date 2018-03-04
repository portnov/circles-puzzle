
module Circles where

import Data.List (nub)
import Graphics.Gloss

import Types

radius :: Float
radius = 100

sqrt32 :: Float
sqrt32 = sqrt 3 / 2

sqrt33 :: Float
sqrt33 = sqrt 3 / 3

sqrt36 :: Float
sqrt36 = sqrt 3 / 6

thickness :: Float
thickness = 2

dt :: Float
dt = 0.01

arcPath dt radius minPhi maxPhi = map go [minPhi, minPhi+dt .. maxPhi]
  where
    go t = (radius * cos t, radius * sin t)

translatePath :: Vector -> Path -> Path
translatePath (dx, dy) path = map go path
  where
    go (x,y) = (x+dx, y+dy)

piece2Path dt radius = 
    (translatePath (0, -radius*sqrt32) $ arcPath dt radius (pi/3) (2*pi/3)) ++
    (translatePath (0,  radius*sqrt32) $ arcPath dt radius (4*pi/3) (5*pi/3))

piece2 :: Float -> Float -> Picture
piece2 dt radius = pictures [ polygon path, color black $ line path ]
  where
    path = piece2Path dt radius

piece3Path dt radius =
  (translatePath (-radius, -radius*sqrt33) $ arcPath dt radius (0) (pi/3)) ++
  (translatePath (0, 2*radius*sqrt32 - radius*sqrt33) $ arcPath dt radius (4*pi/3) (5*pi/3)) ++
  (translatePath (radius, -radius*sqrt33) $ arcPath dt radius (2*pi/3) (pi))

triangle :: Float -> Picture
triangle r = polygon [(r*sqrt33, 0), (-r*sqrt36, r/2), (-r*sqrt36, -r/2)]

piece3 :: Float -> Float -> Picture
piece3 dt radius = pictures [
    color black $ line $ piece3Path dt radius,
    rotate (-30) $ triangle (radius / 2)
  ]

label :: String -> Picture -> Picture
label str p = pictures [p, scale 0.07 0.07 $ color black (text str)]

drawCycle :: [CycleCoordinate] -> Cycle -> Picture
drawCycle exclusions c = pictures $
    map drawRadial (enumerate $ radial c) ++ 
    map drawTriangle (enumerate $ triangles c) ++
    map drawBoundary (enumerate $ boundary c)
  where
    enumerate lst = zip [0..] lst

    radialE = [idx | CycleCoordinate Radial idx <- exclusions]
    triangleE = [idx | CycleCoordinate Triangle idx <- exclusions]
    boundaryE = [idx | CycleCoordinate Boundary idx <- exclusions]

    drawRadial (idx, p) =
      if idx `elem` radialE
        then blank
        else
          rotate (- fromIntegral idx * 60) $
          translate (radius/2) 0 $
          color (pieceColor p) $
          label (pieceText p) $
          piece2 dt radius

    drawTriangle (idx, p) =
      if idx `elem` triangleE
        then blank
        else
          rotate (- fromIntegral idx * 60) $
          translate (radius/2) (sqrt36*radius) $
          rotate 60 $
          color (pieceColor p) $
          label (pieceText p) $
          piece3 dt radius

    drawBoundary (idx, p) = 
      if idx `elem` boundaryE
        then blank
        else
          rotate (- fromIntegral idx * 60) $
          translate (3*radius/4) (radius*sqrt32/2) $
          rotate 60 $
          color (pieceColor p) $
          label (pieceText p) $
          piece2 dt radius

drawField :: Field -> Picture
drawField (Field [c0, c1, c2]) = pictures [c0p, c1p, c2p]
  where
    c0p = translate (radius/2) (-sqrt36*radius) $ drawCycle [] c0

    c1p = translate 0 (sqrt33*radius) $ drawCycle c1exclusions c1

    c2p = translate (-radius/2) (-sqrt36*radius) $ drawCycle c2exclusions c2

    c1exclusions = nub [co | FieldCoordinate 1 co <- map fst allEquations]
    c2exclusions = nub [co | (FieldCoordinate 2 co, FieldCoordinate 0 _) <- allEquations]

