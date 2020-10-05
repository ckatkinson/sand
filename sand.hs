module Main where

import Data.List
import qualified Data.Map.Strict (Map, (!))
import Data.Map.Strict as M hiding (foldl')
import Data.Maybe

type Point = (Int, Int)
type Grains = Int
type Board = Map Point Grains

addPoint :: Num a => (a,a) -> (a,a) -> (a,a)
addPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scalarMult :: Num a => a -> (a,a) -> (a,a)
scalarMult s (x1, y1) = (s*x1, s*y1)

neighbors :: Point -> [Point]
neighbors (x,y) = [(x+1,y), (x-1, y), (x, y+1), (x, y-1)]

topplePoint :: Board -> Point -> Board
topplePoint b pt
  | bAtPt < 4 = b
  | otherwise = M.union (M.fromList (newAtPoint : newNbrs)) b
  where bAtPt = fromMaybe 0 (pt `M.lookup` b)
        newAtPoint = (pt, b!pt - 4)
        newNbrs = [(neigh, newGrains neigh) | neigh <- neighbors pt]
        newGrains :: Point -> Int
        newGrains neigh = 1 + fromMaybe 0 (neigh `M.lookup` b)

topplePoints :: Board -> [Point] -> Board
topplePoints = foldl' topplePoint

isFullyToppled :: Board -> Bool
isFullyToppled b = all ( <4 ) (M.elems b) 
        
topple :: Board -> Board
topple b =
  if isFullyToppled b
    then b
    else topple toppled
    where toppled = topplePoints b (M.keys b)

-- To make a ppm

minX :: Board -> Int
minX b = minimum [ fst p | p <- M.keys b ]

maxX :: Board -> Int
maxX b = maximum [ fst p | p <- M.keys b ]

minY :: Board -> Int
minY b = minimum [ snd p | p <- M.keys b ]

maxY :: Board -> Int
maxY b = maximum [ snd p | p <- M.keys b ]

rectangularize :: Board -> Board
rectangularize b = M.union b zeros
  where zeros = M.fromList [((x,y), 0) | x <- [minX b .. maxX b],
                                         y <- [minY b .. maxY b]]

dimensions :: Board -> (Int, Int)
dimensions b = (maxX b - minX b + 1, maxY b - minY b + 1)

makePpm :: Board -> String -> IO ()
makePpm b s = writeFile s ppm
  where ppm = "P2 " ++ show height ++ " "
                    ++ show width ++ " "
                    ++ "255 "
                    ++ unwords (Data.List.map color (M.elems rect))
        height = fst $ dimensions rect
        width = snd $ dimensions rect
        color x = show ( 255 * x `div` 3)
        rect = rectangularize b

        



b0 :: Board
b0 = M.fromList [((0,0), 17)]

b3 :: Board
b3 = M.fromList [((0,0), 1000)]

b1 :: Board
b1 = M.fromList [((0,0), 100000)]

b2 :: Board
b2 = M.fromList [ (p, 10000) | p<-[(-10,-10),(-10,10),
                                  (10,-10),(10,10),(0,10),(10,0),(-10,0),(0,-10)]]


-- Try to make an stl:


data CellPile = CellPile Point Point Point Point Grains

-- Multiplying coords by 2 so that I can stay in (Int, Int). Note the cyclic
-- order of the vertices. This means that I can triangulate in a predictable way
makeCubey :: Point -> Board -> CellPile 
makeCubey pt bd = CellPile (addPoint pt2 (1, 1)) (addPoint pt2 (-1, 1)) 
                        (addPoint pt2 (-1,-1)) (addPoint pt2 (1, -1)) grains
                        where 
                          pt2 = scalarMult 2 pt
                          grains = bd ! pt

data Triangle = Triangle Point Point Point Grains

-- Probably easiest to add the "vertical" sides here. 
triangulateCell :: CellPile -> [Triangle]
triangulateCell (CellPile p1 p2 p3 p4 g) = [t1, t2]
  where t1 = Triangle p1 p2 p3 g
        t2 = Triangle p3 p4 p1 g

makeBoardTri :: Board -> [Triangle]
makeBoardTri bd = concatMap (\x -> triangulateCell $ makeCubey x bd) (M.keys bd)

triToStlString :: Triangle -> (Int -> Int) -> String
triToStlString (Triangle p1 p2 p3 g) f = unlines ls
  where ls =    ["facet normal 1 0 0",
                 "  outer loop",
                 "    vertex" ++ " " ++ show (f g) ++ " " ++ show (fst p1) ++ " " ++ show (snd p1),
                 "    vertex" ++ " " ++ show (f g) ++ " " ++ show (fst p2) ++ " " ++ show (snd p2),
                 "    vertex" ++ " " ++ show (f g) ++ " " ++ show (fst p3) ++ " " ++ show (snd p3),
                 "  endloop",
                 "endfacet"]

makeStl :: Board -> String -> String -> IO()
makeStl b file name = writeFile file stl
  where stl = "solid " ++ name ++ unlines tristl ++ " endsolid " ++ name
        tris = makeBoardTri b
        tristl = Data.List.map (`triToStlString` f) tris
        -- Can play with different height functions here:
        f = id


-- TODO: 
--    Figure out why ONE triangle seems to be missing (look at 1000.stl, for
--    example)
--
--    Make the vertical sides.  Might have to rewrite a bit. It's a bit annoying
--    to find the sides...



main :: IO ()
--main = makePpm (topple b1) "100k.ppm"
--main = makePpm (topple b2) "corners.ppm"
--main = makeStl (topple b1) "100k.stl" "100k"
--main = makeStl (topple b0) "17.stl" "17"
main = makeStl (topple b3) "1000.stl" "1000"


