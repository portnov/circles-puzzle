
module Types where

import Control.Lens
import Text.Printf
import Data.Monoid
import Data.List (union, nub, (\\))
import Debug.Trace
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

piecesList :: Field -> [Piece]
piecesList fld =
  let rs = [mkRadial fci idx | fci <- [0..2], idx <- [0..5]]
      ts = [mkTriangle fci idx | fci <- [0..2], idx <- [0..5]]
      bs = [mkBoundary fci idx | fci <- [0..2], idx <- [0..5]]
  in  [geteltX co fld | co <- (rs++ts++bs), isCanonical co]

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

geteltX :: FieldCoordinate -> Field -> Piece
geteltX (FieldCoordinate fci co) (Field cycles) = geteltC co (cycles !! fci)

getelt :: FieldCoordinate -> Field -> Piece
getelt fc f = geteltX (canonicalize fc) f

geteltC :: CycleCoordinate -> Cycle -> Piece
geteltC (CycleCoordinate Radial idx) c = radial c !! idx
geteltC (CycleCoordinate Triangle idx) c = triangles c !! idx
geteltC (CycleCoordinate Boundary idx) c = boundary c !! idx

seteltX :: FieldCoordinate -> Piece -> Field -> Field
seteltX (FieldCoordinate fci co) p (Field cycles) = Field $ cycles & element fci %~ seteltC co p

setelt :: FieldCoordinate -> Piece -> Field -> Field
setelt fc p field =
  let setters = [seteltX fc2 p | (fc1, fc2) <- allEquations, fc1 == fc]
  in  seteltX fc p $ appEndo (mconcat $ map Endo setters) field

seteltC :: CycleCoordinate -> Piece -> Cycle -> Cycle
seteltC (CycleCoordinate Radial idx) p c = c {radial = radial c & element idx .~ p}
seteltC (CycleCoordinate Triangle idx) p c = c {triangles = triangles c & element idx .~ p}
seteltC (CycleCoordinate Boundary idx) p c = c {boundary = boundary c & element idx .~ p}

allCanonicalCoordinates :: [FieldCoordinate]
allCanonicalCoordinates =
  let rs = [mkRadial fci idx | fci <- [0..2], idx <- [0..5]]
      ts = [mkTriangle fci idx | fci <- [0..2], idx <- [0..5]]
      bs = [mkBoundary fci idx | fci <- [0..2], idx <- [0..5]]
  in  filter isCanonical (rs ++ ts ++ bs)

rotateList :: [a] -> [a]
rotateList [] = []
rotateList lst = last lst : init lst

rotateCycleClockwise :: Int -> (FieldCoordinate -> FieldCoordinate)
rotateCycleClockwise fci fc = go (toCycle [fci] fc)
  where
    go fc@(FieldCoordinate fci' co@(CycleCoordinate t idx))
        | fci == fci' = fc {fcWithinCycle = CycleCoordinate t ((idx-1) `mod` 6)}
        | otherwise   = fc

rotateCycleCounterClockwise :: Int -> (FieldCoordinate -> FieldCoordinate)
rotateCycleCounterClockwise fci fc = go (toCycle [fci] fc)
  where
    go fc@(FieldCoordinate fci' co@(CycleCoordinate t idx))
        | fci == fci' = fc {fcWithinCycle = CycleCoordinate t ((idx+1) `mod` 6)}
        | otherwise   = fc

apply :: (FieldCoordinate -> FieldCoordinate) -> Field -> Field
apply fn field =
  let setters = [\f -> setelt (fn co) (getelt co field) f | co <- allCanonicalCoordinates]
  in  appEndo (mconcat $ map Endo setters) field

updateCycle :: Int -> [Int] -> [Cycle] -> Cycle
updateCycle targetIdx srcIdxs cycles =
  let field = Field cycles
      getelt' fc = geteltX (toCycle srcIdxs fc) field
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

checkUniq :: Field -> Field
checkUniq f =
  let lst = piecesList f
  in  if nub lst == lst
        then f
        else trace ("Field:\n" ++ show f ++ "\nList: " ++ show lst ++ "\nDiff: " ++ show (lst \\ nub lst)) f

rotateCw :: Int -> Field -> Field
rotateCw idx f = checkUniq $ apply (rotateCycleClockwise idx) f

rotateCcw :: Int -> Field -> Field
rotateCcw idx f = checkUniq $ apply (rotateCycleCounterClockwise idx) f

data Turn = Turn Bool Int
  deriving (Eq, Show)

makeTurn :: Turn -> Field -> Field
makeTurn (Turn True idx) f = rotateCw idx f
makeTurn (Turn False idx) f = rotateCcw idx f

