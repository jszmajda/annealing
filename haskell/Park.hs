module Park (
  Link, Person, Placement, Park
, picnicEnergy, picnicTemperature
, picnicMotion, picnicTransitionalProbability
, similarityLine
, sittingNeighbors, walkingNeighbors
) where

import Polygon (Polygon, Point, distance)
import Data.List
import Data.Ord
import Color
import SimulatedAnnealing as SA
import System.Random (randomR)
import Comb

type Link      = [Point]
type Person    = [Int]
type Placement = [(Point,Person)]
type Park      = [Polygon]

shortestLinks :: Int -> [Link] -> [Link]
shortestLinks n = take n . sortBy (comparing linkLength)
  where
    linkLength [a,b] = distance a b
    linkLength _ = undefined -- exhausting patterns

-- Get the (n * length points) shortest possible connections
sittingNeighbors :: Int -> [Point] -> [Link]
sittingNeighbors n points = nub neighbors
  where
    neighbors  = shortestLinks (n * length points) pointPairs
    pointPairs = combinations 2 points

-- Get n neighbor connections *per point* where neighbors are closest points
walkingNeighbors :: Int -> [Point] -> [Link]
walkingNeighbors n l = nub $ concatMap neighbors l
  where
    neighbors :: Point -> [Link]
    neighbors p = shortestLinks n [[p,a] | a <- l, p /= a]

mismatches :: Person -> Person -> Int
mismatches a b = length $ filter (uncurry (/=)) $ zip a b

similarityColor :: Person -> Person -> Color
similarityColor p1 p2 = let m = mismatches p1 p2
                            h = div (length p1) 2
                            d = 30 * abs (h - m)
                            b = max 0 (255 - d)
                            o = min d 255
                        in if m < h
                           then (0,o,b)
                           else (o,0,b)

findPerson :: Placement -> Point -> Person
findPerson placement p
  | Just (_, person) <- find ((== p) . fst) placement = person
  | otherwise = undefined -- exhausting patterns

similarityLine :: Placement -> Link -> (Color,Polygon)
similarityLine placement [p1,p2] = (color, line)
  where
    color  = similarityColor (person p1) (person p2)
    line   = [p1,p2]
    person = findPerson placement

similarityLine _ _ = undefined -- exhausting patterns

-- Annealing time!

picnicEnergy :: [Link] -> SA.EnergyFunction Placement
picnicEnergy l a = sum $ map linkEnergy l
  where linkEnergy :: Link -> Int
        linkEnergy [p1,p2] = mismatches (findPerson a p1) (findPerson a p2)
        linkEnergy _ = undefined -- exhausting patterns

picnicMotion :: [Link] -> SA.MotionFunction Placement
picnicMotion l r a = let (n,r2) = randomR (0, length l - 1) r
                         [p1,p2] = l!!n
                     in (r2, (p1, findPerson a p2) : (p2, findPerson a p1) : filter (not . flip elem [p1,p2] . fst) a)

picnicTemperature :: SA.TemperatureFunction
picnicTemperature m c = 50.0 * exp (0.0 - (5.0 * (fromIntegral c / fromIntegral m)))

picnicTransitionalProbability :: SA.TransitionProbabilityFunction
picnicTransitionalProbability e1 e2 t = exp (fromIntegral (e1 - e2) / t)

