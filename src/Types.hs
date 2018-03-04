
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
  deriving (Eq)

instance Show Piece where
  show p = pieceText p

data Cycle = Cycle {
    radial :: [Piece],
    triangles :: [Piece],
    boundary :: [Piece]
  }
  deriving (Eq)

instance Show Cycle where
  show c = printf "Cycle\nradial:\t%s\ntriangles:\t%s\nboundary:\t%s\n"
             (show $ radial c)
             (show $ triangles c)
             (show $ boundary c)

data Field = Field [Cycle]
  deriving (Eq)

instance Show Field where
  show (Field cycles) = concat $ map show cycles

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

mkRadial :: Int -> Int -> FieldCoordinate
mkRadial c k = FieldCoordinate c (CycleCoordinate Radial k)

mkTriangle :: Int -> Int -> FieldCoordinate
mkTriangle c k = FieldCoordinate c (CycleCoordinate Triangle k)

mkBoundary :: Int -> Int -> FieldCoordinate
mkBoundary c k = FieldCoordinate c (CycleCoordinate Boundary k)

equations :: [Equation]
equations =
    [
      (mkRadial 0 1, mkBoundary 1 5),
      (mkRadial 0 2, mkBoundary 2 0),
      (mkRadial 0 2, mkRadial 1 5),
      (mkRadial 0 3, mkRadial 2 0),
      (mkRadial 0 3, mkBoundary 1 4),
      (mkRadial 0 4, mkBoundary 2 5),
      (mkTriangle 0 1, mkTriangle 1 5),
      (mkTriangle 0 2, mkTriangle 1 4),
      (mkTriangle 0 2, mkTriangle 2 0),
      (mkTriangle 0 3, mkTriangle 2 5),
      (mkBoundary 0 1, mkRadial 1 0),
      (mkBoundary 0 2, mkRadial 2 1),
      (mkBoundary 0 2, mkRadial 1 4),
      (mkBoundary 0 3, mkRadial 2 5),
      (mkRadial 1 3, mkBoundary 2 1),
      (mkTriangle 1 3, mkTriangle 2 1),
      (mkBoundary 1 3, mkRadial 2 2)
    ]

allEquations :: [Equation]
allEquations = equations `union` map swp equations
  where
    swp (c1,c2) = (c2, c1)

isCanonical :: FieldCoordinate -> Bool
isCanonical fc@(FieldCoordinate 0 _) = True
isCanonical fc@(FieldCoordinate 1 _) = fc `notElem` map fst allEquations
isCanonical fc@(FieldCoordinate 2 _) = not $ any check allEquations
  where
    check (fc2, FieldCoordinate 0 _) = fc2 == fc
    check _ = False


canonicalize :: FieldCoordinate -> FieldCoordinate
canonicalize fc =
  case lookup fc allEquations of
    Nothing -> fc
    Just fc' -> if isCanonical fc'
                  then fc'
                  else canonicalize fc'

isCycle :: [Int] -> FieldCoordinate -> Bool
isCycle fcis (FieldCoordinate fci _) = fci `elem` fcis

toCycle :: [Int] -> FieldCoordinate -> FieldCoordinate
toCycle fcis fc =
    case find allEquations of
      Nothing -> fc
      Just fc' -> if isCycle fcis fc'
                    then fc'
                    else toCycle fcis fc'
  where
    find [] = Nothing
    find ((src,dst):eqs)
      | fcCycle dst `elem` fcis && src == fc = Just dst
      | otherwise = find eqs

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

updateCycle :: Int -> [Int] -> [Cycle] -> Cycle
updateCycle targetIdx srcIdxs cycles =
  let field = Field cycles
      getelt' fc = getelt (toCycle srcIdxs fc) field
  in Cycle {
       radial = map getelt' [mkRadial targetIdx idx | idx <- [0..5]],
       triangles = map getelt' [mkTriangle targetIdx idx | idx <- [0..5]],
       boundary = map getelt' [mkBoundary targetIdx idx | idx <- [0..5]]
     }

mkCycle :: Color -> Int -> Cycle
mkCycle color fci = Cycle {
       radial = [Piece Two color (show fci++"R"++show idx) | idx <- [0..5]],
       triangles = [Piece Three color (show fci++"T"++show idx) | idx <- [0..5]],
       boundary = [Piece Two color (show fci++"B"++show idx) | idx <- [0..5]]
     }

initialField :: Field
initialField = Field [c0, c1, c2]
  where
    c0 = mkCycle red 0

    c1_ = mkCycle green 1
    c2_ = mkCycle blue 2

    c1 = updateCycle 1 [0,2] [c0, c1_, c2]
    c2 = updateCycle 2 [0] [c0, c1_, c2_]

rotate0 :: Field -> Field
rotate0 (Field [c0,c1,c2]) = Field [c0', c1', c2']
  where
    c0' = rotateCycle c0
    c1' = updateCycle 1 [0] [c0', c1, c2]
    c2' = updateCycle 2 [0] [c0', c1, c2]

rotate1 :: Field -> Field
rotate1 (Field [c0, c1, c2]) = Field [c0', c1', c2']
  where
    c0' = updateCycle 0 [1] [c0, c1', c2]
    c1' = rotateCycle c1
    c2' = updateCycle 2 [1] [c0, c1', c2]

rotate2 :: Field -> Field
rotate2 (Field [c0, c1, c2]) = Field [c0', c1', c2']
  where
    c0' = updateCycle 0 [2] [c0, c1, c2']
    c1' = updateCycle 1 [2] [c0, c1, c2']
    c2' = rotateCycle c2

