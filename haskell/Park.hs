module Park (
  Link, Person, Placement, Park, People
, shortestLinks, sittingNeighbors, walkingNeighbors
, similarityLine
, picnicEnergy, picnicTemperature
, picnicMotion, picnicTransitionalProbability
) where

import Polygon (Polygon, Point, distance)
import Data.List
import Text.Regex
import Data.Ord
import Color
import SimulatedAnnealing as SA
import System.Random (randomR)

type Link      = [Point]
type Person    = [Int]
type People    = [Person]
type Placement = [(Point,Person)]
type Park      = [Polygon]

shortestLinks :: Int -> [Link] -> [Link]
shortestLinks n = (take n).(sortBy $ comparing linkLength)
 where linkLength [a,b] = distance a b

sittingNeighbors :: Int -> [Point] -> [Link]
sittingNeighbors n p = nub $ shortestLinks (n * (length p)) [[a,b] | a <- p, b <- p, a /= b]

walkingNeighbors :: Int -> [Point] -> [Link]
walkingNeighbors n l = nub $ concatMap myNeighbors l
    where myNeighbors :: Point -> [Link]
          myNeighbors p = shortestLinks n [sort [p,c] | c <- l, p /= c]

mismatches :: Person -> Person -> Int
mismatches a b = length $ filter (uncurry (/=)) $ zip a b

similarityColor :: Person -> Person -> Color
similarityColor p1 p2 = let m = mismatches p1 p2
                            h = div (length p1) 2
                            d = 30 * (abs (h - m))
                            b = max 0 (255-d)
                            o = min d 255
                        in if m < h
                           then (0,o,b)
                           else (o,0,b)

findPerson :: Placement -> Point -> Person
findPerson a p | Just (_,e) <- find ((== p).fst) a = e

similarityLine :: Placement -> Link -> (Color,Polygon)
similarityLine l [p1,p2] = (similarityColor (findPerson l p1) (findPerson l p2),[p1,p2])

-- Annealing time!

picnicEnergy :: [Link] -> SA.EnergyFunction Placement
picnicEnergy l a = sum $ map linkEnergy l
    where linkEnergy :: Link -> Int
          linkEnergy [p1,p2] = mismatches (findPerson a p1) (findPerson a p2)

picnicMotion :: [Link] -> SA.MotionFunction Placement
picnicMotion l r a = let (n,r2) = randomR (0,(length l)-1) r
                         [p1,p2] = l!!n
                     in (r2,(p1,findPerson a p2):(p2,findPerson a p1):(filter (not.((flip elem) [p1,p2]).fst) a))

picnicTemperature :: SA.TemperatureFunction
picnicTemperature m c = 50.0 * (exp (0.0 - (5.0 * ((fromIntegral c) / (fromIntegral m)))))

picnicTransitionalProbability :: SA.TransitionProbabilityFunction
picnicTransitionalProbability e1 e2 t = exp ((fromIntegral (e1 - e2)) / t)

