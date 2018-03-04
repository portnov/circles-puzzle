
module Types where

import Control.Lens
import Text.Printf
import Data.List (union)
import Graphics.Gloss

data PieceKind = Two | Three
  deriving (Eq, Show, Ord)

data Piece = Piece {
    pieceKind :: PieceKind,
    pieceColor :: Color,
    pieceText :: String
  }
  deriving (Eq, Show)

data Cycle = Cycle {
    radial :: [Piece],
    triangles :: [Piece],
    boundary :: [Piece]
  }
  deriving (Eq, Show)

data Field = Field [Cycle]
  deriving (Eq, Show)

data PieceLocation = Radial | Triangle | Boundary
  deriving (Eq, Show)

data CycleCoordinate = CycleCoordinate {
    pcLocation :: PieceLocation,
    pcIndex :: Int
  }
  deriving (Eq)

instance Show CycleCoordinate where
  show (CycleCoordinate loc idx) =
    printf "%s[%s]" (show loc) (show idx)

data FieldCoordinate = FieldCoordinate {
    fcCycle :: Int,
    fcWithinCycle :: CycleCoordinate
  }
  deriving (Eq)

instance Show FieldCoordinate where
  show (FieldCoordinate fci (CycleCoordinate loc idx)) =
    printf "%s[%s][%s]" (show loc) (show fci) (show idx)

type Equation = (FieldCoordinate, FieldCoordinate)

equations :: [Equation]
equations =
  let radial c k = FieldCoordinate c (CycleCoordinate Radial k)
      triangle c k = FieldCoordinate c (CycleCoordinate Triangle k)
      boundary c k = FieldCoordinate c (CycleCoordinate Boundary k)
  in [
      (radial 0 1, boundary 1 5),
      (radial 0 2, boundary 2 0),
      (radial 0 2, radial 1 5),
      (radial 0 3, radial 2 0),
      (radial 0 3, boundary 1 4),
      (radial 0 4, boundary 2 5),
      (triangle 0 1, triangle 1 5),
      (triangle 0 2, triangle 1 4),
      (triangle 0 2, triangle 2 0),
      (triangle 0 3, triangle 2 5),
      (boundary 0 1, radial 1 0),
      (boundary 0 2, radial 2 1),
      (boundary 0 2, radial 1 4),
      (boundary 0 3, radial 2 5),
      (radial 1 3, boundary 2 1),
      (triangle 1 3, triangle 2 1),
      (boundary 1 3, radial 2 2)
    ]

allEquations :: [Equation]
allEquations = equations `union` map swp equations
  where
    swp (c1,c2) = (c2, c1)

getelt :: FieldCoordinate -> Field -> Piece
getelt (FieldCoordinate fci co) (Field cycles) = geteltC co (cycles !! fci)

geteltC :: CycleCoordinate -> Cycle -> Piece
geteltC (CycleCoordinate Radial idx) c = radial c !! idx
geteltC (CycleCoordinate Triangle idx) c = triangles c !! idx
geteltC (CycleCoordinate Boundary idx) c = boundary c !! idx

setelt :: FieldCoordinate -> Piece -> Field -> Field
setelt (FieldCoordinate fci co) p (Field cycles) = Field $ cycles & element fci %~ seteltC co p

seteltC :: CycleCoordinate -> Piece -> Cycle -> Cycle
seteltC (CycleCoordinate Radial idx) p c = c {radial = radial c & element idx .~ p}
seteltC (CycleCoordinate Triangle idx) p c = c {triangles = triangles c & element idx .~ p}
seteltC (CycleCoordinate Boundary idx) p c = c {boundary = boundary c & element idx .~ p}

rotateList :: [a] -> [a]
rotateList [] = []
rotateList lst = last lst : init lst

rotateCycle :: Cycle -> Cycle
rotateCycle c =
  c {
    radial = rotateList (radial c),
    triangles = rotateList (triangles c),
    boundary = rotateList (boundary c)
  }

updateCycle :: Int -> [Cycle] -> Cycle
updateCycle fci cycles =
  let ourEquations = filter our allEquations
      our (fc,_) = fcCycle fc == fci
      radialEqs = filter (isRadial . fst) ourEquations
      triangleEqs = filter (isTriangle . fst) ourEquations
      boundaryEqs = filter (isBoundary . fst) ourEquations

      isRadial (FieldCoordinate _ (CycleCoordinate Radial _)) = True
      isRadial _ = False

      isTriangle (FieldCoordinate _ (CycleCoordinate Triangle _)) = True
      isTriangle _ = False

      isBoundary (FieldCoordinate _ (CycleCoordinate Boundary _)) = True
      isBoundary _ = False

      apply :: [Equation] -> FieldCoordinate -> Piece
      apply eqs fc = case lookup fc eqs of
                       Nothing -> getelt fc (Field cycles)
                       Just fc' -> getelt fc' (Field cycles)
  in Cycle {
       radial = map (apply radialEqs) [FieldCoordinate fci (CycleCoordinate Radial idx) | idx <- [0..5]],
       triangles = map (apply triangleEqs) [FieldCoordinate fci (CycleCoordinate Triangle idx) | idx <- [0..5]],
       boundary = map (apply boundaryEqs) [FieldCoordinate fci (CycleCoordinate Boundary idx) | idx <- [0..5]]
     }

initialField :: Field
initialField = Field [c0, c1, c2]
  where
    mkCycle color fci = Cycle {
           radial = [Piece Two color (show fci++"R"++show idx) | idx <- [0..5]],
           triangles = [Piece Three color (show fci++"T"++show idx) | idx <- [0..5]],
           boundary = [Piece Two color (show fci++"B"++show idx) | idx <- [0..5]]
         }

    c0 = mkCycle red 0

    c1_ = mkCycle green 1
    c2_ = mkCycle blue 2

    c1 = updateCycle 1 [c0, c1_, c2_]
    c2 = updateCycle 2 [c0, c1_, c2_]

rotate0 :: Field -> Field
rotate0 (Field [c0,c1,c2]) = Field [c0', c1', c2']
  where
    c0' = rotateCycle c0
    c1' = updateCycle 1 [c0', c1, c2]
    c2' = updateCycle 2 [c0', c1, c2]

-- rotate2 :: Field -> Field
-- rotate2 f = f {
--       fCycle0 = update0 
--       fCycle1 = 
--       fCycle2 = 
--     }
--   where
--     r = rotateList (fCycle2 f)
