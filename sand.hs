module Main where

import Data.List
import qualified Data.Map.Strict (Map, (!))
import Data.Map.Strict as M hiding (foldl', map)
import Data.Maybe

type Point = (Int, Int)
type Grains = Int
type Board = Map Point Grains

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
-- topplePoints b [] = b
-- topplePoints b (p:ps) = topplePoints (topplePoint b p) ps

isFullyToppled :: Board -> Bool
isFullyToppled b = all ( <4 ) (M.elems b) 
        
topple :: Board -> Board
topple b =
  if isFullyToppled b
    then b
    else topple toppled
    where toppled = topplePoints b (M.keys b)

-- To make a pbm

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
                    ++ unwords (map color (M.elems rect))
        height = fst $ dimensions rect
        width = snd $ dimensions rect
        color x = show ( 255 * x `div` 3)
        rect = rectangularize b

b0 :: Board
b0 = M.fromList [((0,0), 17)]

b1 :: Board
b1 = M.fromList [((0,0), 100000)]

main :: IO ()
main = makePpm (topple b1) "100k.ppm"


