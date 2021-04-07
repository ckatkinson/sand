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
neighbors (x,y) = [(x+1,y), (x, y+1), (x-1, y), (x, y-1)]

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
b0 = M.fromList [((0,0), 100)]

b3 :: Board
b3 = M.fromList [((0,0), 1000)]

b4 :: Board
b4 = M.fromList [((0,0), 10000)]

b1 :: Board
b1 = M.fromList [((0,0), 100000)]

b1M :: Board
b1M = M.fromList [((0,0), 1000000)]

b2 :: Board
b2 = M.fromList [ (p, 10000) | p<-[(-10,-10),(-10,10),
                                  (10,-10),(10,10),(0,10),(10,0),(-10,0),(0,-10)]]


-- Try to make an stl:


data CellPile = CellPile Point Point Point Point Grains deriving Show
data Direction = L | U | R | D deriving Show
data DiagDir = LU | LD | RU | RD deriving Show
type Point3d = (Int, Int, Int)


--- WARNING. This is a clusterfuck right now. Refactoring a bit to get the
--"vertical" rectangles in. 

dirToVect :: DiagDir -> (Int, Int)
dirToVect LU = (-1, 1)
dirToVect LD = (-1, -1)
dirToVect RU = (1, 1)
dirToVect RD = (1, -1)

edge :: Point -> Direction -> (Point, Point)
edge pt L = (addPoint pt (-1, -1), addPoint pt (-1, 1))
edge pt U = (addPoint pt (-1, 1), addPoint pt (1, 1))
edge pt R = (addPoint pt (1, -1), addPoint pt (1, 1))
edge pt D = (addPoint pt (-1, -1), addPoint pt (1, -1))

-- Multiplying coords by 2 so that I can stay in (Int, Int). Note the cyclic
-- order of the vertices. This means that I can triangulate in a predictable way
makeCellPile :: Point -> Board -> CellPile 
makeCellPile pt bd = CellPile (addPoint pt2 (1, 1)) (addPoint pt2 (-1, 1)) 
                        (addPoint pt2 (-1,-1)) (addPoint pt2 (1, -1)) grains
                        where 
                          pt2 = scalarMult 2 pt
                          grains = bd ! pt


heightNeighbor:: Point -> Direction -> Board -> Int
heightNeighbor p1 L bd = fromMaybe (-4) (addPoint p1 (-1,0) `M.lookup` bd)
heightNeighbor p1 U bd = fromMaybe (-4) (addPoint p1 (0, 1) `M.lookup` bd)
heightNeighbor p1 R bd = fromMaybe (-4) (addPoint p1 (1, 0) `M.lookup` bd)
heightNeighbor p1 D bd = fromMaybe (-4) (addPoint p1 (0, -1) `M.lookup` bd)


data Rectangle = Rectangle Point3d Point3d Point3d Point3d deriving Show
type RectNormal = (Rectangle, Direction)

normal :: Direction -> Point3d
normal R = (1, 0, 0)
normal U = (0, 1, 0)
normal L = (-1, 0, 0)
normal D = (0, -1, 0)

showNormal :: Point3d -> String
showNormal (x, y, z) = " " ++ show x ++ " " ++ show y ++ " " ++ show z

getx :: Point3d -> Int
getx (x, _, _ ) = x

gety :: Point3d -> Int
gety ( _, y, _ ) = y

getz :: Point3d -> Int
getz ( _, _, z ) = z

makeTrip :: Point -> Int -> Point3d
makeTrip (x,y) z = (x,y,z)

doublePoint :: Point -> Point
doublePoint (x,y) = (2*x, 2*y)

buildRectNormal :: Point -> Board -> (Int -> Int) -> Direction -> Maybe RectNormal
buildRectNormal pt bd f dir = rn
  where height = f $ bd ! pt
        e = edge (doublePoint pt) dir
        hd = f $ heightNeighbor pt dir bd
        rn = if hd < height
               then Just (Rectangle (makeTrip (fst e) height) (makeTrip (snd e) height)   
                                    (makeTrip (snd e) hd) (makeTrip (fst e) hd), dir)
               else Nothing

verticalRectangles :: Point -> Board -> (Int -> Int) -> [Maybe RectNormal]
verticalRectangles pt bd f = rects
  where rects = Data.List.map (buildRectNormal pt bd f) [R, U, L, D]

data Triangle = Triangle Point3d Point3d Point3d deriving Show

triangulateRectangle :: RectNormal -> [(Triangle, Direction)]
triangulateRectangle (Rectangle p1 p2 p3 p4, dir) = [t1, t2]
  where t1 = (Triangle p1 p2 p3, dir)
        t2 = (Triangle p1 p3 p4, dir)

triangulateCell :: CellPile -> [Triangle]
triangulateCell (CellPile p1 p2 p3 p4 g) = [t1, t2]
  where t1 = Triangle (makeTrip p1 g) (makeTrip p2 g) (makeTrip p3 g) 
        t2 = Triangle (makeTrip p3 g) (makeTrip p4 g) (makeTrip p1 g) 

makeBoardTriHoriz :: Board -> [Triangle]
makeBoardTriHoriz bd = horiz
  where horiz = concatMap (\x -> triangulateCell $ makeCellPile x bd) (M.keys bd)


makeBoardTriVert :: Board -> (Int -> Int) -> [(Triangle, Direction)]
makeBoardTriVert bd f = vert
  where vert  = concatMap (\x -> concatMap triangulateRectangle $ catMaybes (verticalRectangles x bd f)) (M.keys bd)

triToStlString :: Triangle -> (Int -> Int) -> String
triToStlString (Triangle p1 p2 p3) f = unlines ls
  where ls =    ["facet normal 0 0 1",
                 "  outer loop",
                 "    vertex"  ++ " " ++ show (getx p1) ++ " " ++ show (gety p1) ++ " " ++ show (f (getz p1)),
                 "    vertex"  ++ " " ++ show (getx p2) ++ " " ++ show (gety p2) ++ " " ++ show (f (getz p2)),
                 "    vertex"  ++ " " ++ show (getx p3) ++ " " ++ show (gety p3) ++ " " ++ show (f (getz p3)),
                 "  endloop",
                 "endfacet"]
                 
baseTriToStlString :: Triangle -> (Int -> Int) -> String
baseTriToStlString (Triangle p1 p2 p3) f = unlines ls
  where ls =    ["facet normal 0 0 -1",
                 "  outer loop",
                 "    vertex"  ++ " " ++ show (getx p1) ++ " " ++ show (gety p1) ++ " " ++ show (f (getz p1)),
                 "    vertex"  ++ " " ++ show (getx p2) ++ " " ++ show (gety p2) ++ " " ++ show (f (getz p2)),
                 "    vertex"  ++ " " ++ show (getx p3) ++ " " ++ show (gety p3) ++ " " ++ show (f (getz p3)),
                 "  endloop",
                 "endfacet"]


vertTriToStlString :: (Triangle, Direction) -> String
vertTriToStlString (Triangle p1 p2 p3, dir) = unlines ls
  where ls =    ["facet normal " ++ showNormal (normal dir),
                 "  outer loop",
                 "    vertex"  ++ " " ++ show (getx p1) ++ " " ++ show (gety p1) ++ " " ++ show (getz p1),
                 "    vertex"  ++ " " ++ show (getx p2) ++ " " ++ show (gety p2) ++ " " ++ show (getz p2),
                 "    vertex"  ++ " " ++ show (getx p3) ++ " " ++ show (gety p3) ++ " " ++ show (getz p3),
                 "  endloop",
                 "endfacet"]

cellPileToStlString :: CellPile -> (Int -> Int) -> String 
cellPileToStlString cp f = unlines (ss ++ zz)
  where triCell = triangulateCell cp
        ss = Data.List.map (`triToStlString` f) triCell
        zz = Data.List.map (`triToStlString` const (-4)) triCell

makeStl :: Board -> String -> String -> IO()
makeStl b file name = writeFile file stl
  where stl = "solid " ++ name ++ unlines tristlH ++ unlines trisBase ++ unlines tristlV ++ " endsolid " ++ name
        trisHoriz = makeBoardTriHoriz b 
        tristlH = Data.List.map (`triToStlString` f) trisHoriz
        trisBase = Data.List.map (`baseTriToStlString` const (-4)) trisHoriz
        trisVert = makeBoardTriVert b f
        tristlV = Data.List.map vertTriToStlString trisVert
        -- Can play with different height functions here:
        f x = 2*x


-- TODO: 
--    Figure out why ONE triangle seems to be missing (look at 1000.stl, for
--    example)


main :: IO ()
--main = makePpm (topple b1) "100k.ppm"
--main = makePpm (topple b2) "corners.ppm"
--main = makeStl (topple b1) "100k.stl" "100k"
main = makeStl (topple b0) "10.stl" "10"
--main = makeStl (topple b3) "1000.stl" "1000"
--main = makeStl (topple b4) "10000.stl" "10000"
--main = makeStl (topple b1M) "1M.stl" "1000000"


