module Polygon (
  Point, Polygon
, triangulate, clipTriangle
, distance, slice, sliceX, sliceY
, allocatePeople, findLotCenter, makeDot, centers
) where

import Data.List (partition, minimumBy, intercalate)
import Data.Ord (comparing)
import Debug.Trace
import Text.Printf

type Point     = (Float,Float)
type Polygon   = [Point]

triangulate :: Polygon -> [Polygon]
triangulate (a:b:c:xs) = [a,b,c]:triangulate (a:c:xs)
triangulate _ = []

clipTriangle :: (Point -> Point -> Point) -> [Point] -> [Point] -> [Polygon]
clipTriangle _ [] [_,_,_] = []
clipTriangle f [a]  [b,c] = [[a,f a b,f a c]]
clipTriangle f [a,b]  [c] = [[a,f a c,b],[b,f a c,f b c]]
clipTriangle _ [a,b,c] [] = [[a,b,c]]
clipTriangle _ _ _ = undefined -- exhausting patterns

slice :: (Point -> Bool) -> (Point -> Point -> Point) -> [Polygon] -> ([Polygon],[Polygon])
slice part midpoint triangles = (clip part,clip $ not.part)
  where clip g = concatMap (uncurry (clipTriangle midpoint) . partition g) triangles

sliceX :: Float -> [Polygon] -> ([Polygon],[Polygon])
sliceX x = slice ((x >) . fst) midX
  where midX (x1,y1) (x2,y2) = (x,y1+(y2-y1)*(x-x1)/(x2-x1))

sliceY :: Float -> [Polygon] -> ([Polygon],[Polygon])
sliceY y = slice ((y >) . snd) midY
  where midY (x1,y1) (x2,y2) = (x1+(x2-x1)*(y-y1)/(y2-y1),y)


boundingRect :: [Polygon] -> (Float,Float,Float,Float)
boundingRect p = (minimum xs,minimum ys,maximum xs,maximum ys)
    where xs = map fst $ concat p
          ys = map snd $ concat p

halveTriangles :: Int -> [Polygon] -> ([Polygon],[Polygon])
halveTriangles n p = let (l,t,r,b) = boundingRect p
                         f         = fromIntegral n
                         h         = fromIntegral $ div n 2
                     in if r-l > b-t
                        then sliceX ((r*h+l*(f-h))/f) p
                        else sliceY ((b*h+t*(f-h))/f) p

distance :: Point -> Point -> Float
distance p1 p2 = sqrt (dx * dx + dy * dy)
    where dx = fst p1 - fst p2
          dy = snd p1 - snd p2

area :: Polygon -> Float
area [a,b,c] = let x = distance a b
                   y = distance b c
                   z = distance c a
                   s = (x + y + z) / 2
               in sqrt (s * (s - x) * (s - y) * (s - z))
area _ = undefined -- exhausting patterns

allocatePeople :: Int -> [Polygon] -> [[Polygon]]
allocatePeople 0 _ = []
allocatePeople 1 gon = [gon]
allocatePeople n gon = let (t1,t2) = halveTriangles n gon
                           a1      = sum $ map area t1
                           a2      = sum $ map area t2
                           f = trace "---" $ traceShow n $ traceShow t2 $ traceShow t1 $ traceShowId $ trace ("a1: " ++ (show a1) ++ " a2: " ++ (show a2)) $ traceShow gon $ round $ fromIntegral n * a1 / (a1 + a2)
                     in allocatePeople f t1 ++ allocatePeople (n - f) t2

centers :: [Polygon] -> Int -> [Point]
centers polys num = map findLotCenter lots
  where
    -- lots      = traceShowId' $ allocatePeople num triangles
    lots      = allocatePeople num triangles
    triangles = concatMap triangulate polys

findLotCenter :: [Polygon] -> Point
findLotCenter p = let (l,t,r,b) = boundingRect p
                      m@(x,y)   = ((r+l)/2,(b+t)/2)
                      (lh,rh)   = sliceX x p
                      (th,bh)   = sliceY y $ lh ++ rh
                  in minimumBy (comparing $ distance m) $ concat $ th ++ bh

makeDot :: Point -> Polygon
makeDot (x,y) = [(x - 2,y - 2), (x + 2,y - 2), (x + 2,y + 2), (x - 2,y + 2)]

traceShowId' :: [[Polygon]] -> [[Polygon]]
traceShowId' a = trace (vval a) a

vval :: [[Polygon]] -> String
vval = concatMap shpg
  where
    shpg pg = "[" ++ concatMap shp pg ++ "\n]\n"
    shp p   = "\n  [" ++ intercalate "," (map sht p) ++ "]"
    sht (x,y) = "(" ++ cc x ++ "," ++ cc y ++ ")"
    cc = printf "%0.3f"
