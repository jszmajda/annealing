module Polygon (
  Point, Polygon
, triangulate, clipTriangle
, distance, slice, sliceX, sliceY
, allocatePeople, findLotCenter, makeDot, centers
) where

import Data.List (partition, minimumBy)
import Data.Ord (comparing)

type Point     = (Float,Float)
type Polygon   = [Point]

triangulate :: Polygon -> [Polygon]
triangulate (a:b:c:xs) = [a,b,c]:triangulate (a:c:xs)
triangulate _ = []

clipTriangle :: (Point -> Point -> Point) -> [Point] -> [Point] -> [Polygon]
clipTriangle i [] [a,b,c] = []
clipTriangle i [a]  [b,c] = [[a,i a b,i a c]]
clipTriangle i [a,b]  [c] = [[a,i a c,b],[b,i a c,i b c]]
clipTriangle i [a,b,c] [] = [[a,b,c]]

slice :: (Point -> Bool) -> (Point -> Point -> Point) -> [Polygon] -> ([Polygon],[Polygon])
slice f i t = (clip f,clip $ not.f)
  where clip g = concatMap ((uncurry $ clipTriangle i).(partition g)) t

sliceX :: Float -> [Polygon] -> ([Polygon],[Polygon])
sliceX x = slice ((x >).fst) interpolateX
  where interpolateX (x1,y1) (x2,y2) = (x,y1+(y2-y1)*(x-x1)/(x2-x1))

sliceY :: Float -> [Polygon] -> ([Polygon],[Polygon])
sliceY y = slice ((y >).snd) interpolateY
  where interpolateY (x1,y1) (x2,y2) = (x1+(x2-x1)*(y-y1)/(y2-y1),y)


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
distance p1 p2 = sqrt (deltax*deltax+deltay*deltay)
    where deltax = (fst p1)-(fst p2)
          deltay = (snd p1)-(snd p2)

area :: Polygon -> Float
area [a,b,c] = let x = distance a b
                   y = distance b c
                   z = distance c a
                   s = (x+y+z)/2
               in sqrt (s*(s-x)*(s-y)*(s-z))

allocatePeople :: Int -> [Polygon] -> [[Polygon]]
allocatePeople 0 t = []
allocatePeople 1 t = [t]
allocatePeople n t = let (t1,t2) = halveTriangles n t
                         a1      = sum $ map area t1
                         a2      = sum $ map area t2
                         f = round $ (fromIntegral n)*a1/(a1+a2)
                     in (allocatePeople f t1)++(allocatePeople (n-f) t2)

centers :: [Polygon] -> [a] -> [Point]
centers polys list = map findLotCenter lots
  where
    lots      = allocatePeople (length list) triangles
    triangles = concatMap triangulate polys

findLotCenter :: [Polygon] -> Point
findLotCenter p = let (l,t,r,b) = boundingRect p
                      m@(x,y)   = ((r+l)/2,(b+t)/2)
                      (lh,rh)   = sliceX x p
                      (th,bh)   = sliceY y $ lh ++ rh
                      centerOrder p1 p2 = compare (distance p1 m) (distance p2 m)
                  in minimumBy (comparing $ distance m) $ concat $ th ++ bh

makeDot :: Point -> Polygon
makeDot (x,y) = [(x-2,y-2),(x+2,y-2),(x+2,y+2),(x-2,y+2)]
