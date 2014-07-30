import Data.List
import Text.Regex
import System.Random
import Data.Ord

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Polygon   = [Point]
type Person    = [Int]
type Link      = [Point]
type Placement = [(Point,Person)]

type EnergyFunction a              = a -> Int
type TemperatureFunction           = Int -> Int -> Float
type TransitionProbabilityFunction = Int -> Int -> Float -> Float
type MotionFunction a              = StdGen -> a -> (StdGen,a)

main = do
  putStr "Hello World! Let's have a picnic! \n"

  people_text <- readFile "people.txt"

  let people :: [Person]
      people = read people_text

  putStr "Number of people coming: "
  print (length people)

  let writePoint :: Point -> String
      writePoint (x,y) = (show x)++","++(show y)++" "

  let writePolygon :: (Color,Polygon) -> String
      writePolygon ((r,g,b),p) = "<polygon points=\""++(concatMap writePoint p)++"\" style=\"fill:#cccccc;stroke:rgb("++(show r)++","++(show g)++","++(show b)++");stroke-width:2\"/>"

  let writePolygons :: [(Color,Polygon)] -> String
      writePolygons p = "<svg xmlns=\"http://www.w3.org/2000/svg\">"++(concatMap writePolygon p)++"</svg>"

  let colorize :: Color -> [Polygon] -> [(Color,Polygon)]
      colorize = zip.repeat

  let rainbow@[red,green,blue,yellow,purple,teal] = map colorize [(255,0,0),(0,255,0),(0,0,255),(255,255,0),(255,0,255),(0,255,255)]

  writeFile "tut0.svg" $ writePolygons (blue [[(100,100),(200,100),(200,200),(100,200)],[(200,200),(300,200),(300,300),(200,300)]])

  let readPoint :: String -> Point
      readPoint s | Just [x,y] <- matchRegex (mkRegex "([0-9.]+),([0-9.]+)") s = (read x,read y)

  let readPolygon :: String -> Polygon
      readPolygon = (map readPoint).(splitRegex $ mkRegex " L ")

  let readPolygons :: String -> [Polygon]
      readPolygons = (map readPolygon).tail.(splitRegex $ mkRegex "<path")

  park_data <- readFile "park.svg"

  let park = readPolygons park_data

  writeFile "tut1.svg" $ writePolygons (green park)

  let triangulate :: Polygon -> [Polygon]
      triangulate (a:b:c:xs) = [a,b,c]:triangulate (a:c:xs)
      triangulate _ = []

  let triangles = concatMap triangulate park

  writeFile "tut2.svg" $ writePolygons (purple triangles)

  let clipTriangle :: (Point -> Point -> Point) -> [Point] -> [Point] -> [Polygon]
      clipTriangle i [] [a,b,c] = []
      clipTriangle i [a]  [b,c] = [[a,i a b,i a c]]
      clipTriangle i [a,b]  [c] = [[a,i a c,b],[b,i a c,i b c]]
      clipTriangle i [a,b,c] [] = [[a,b,c]]

  let slice :: (Point -> Bool) -> (Point -> Point -> Point) -> [Polygon] -> ([Polygon],[Polygon])
      slice f i t = (clip f,clip $ not.f)
          where clip g = concatMap ((uncurry $ clipTriangle i).(partition g)) t

  let sliceX :: Float -> [Polygon] -> ([Polygon],[Polygon])
      sliceX x = slice ((x >).fst) interpolateX
          where interpolateX (x1,y1) (x2,y2) = (x,y1+(y2-y1)*(x-x1)/(x2-x1))

  let sliceY :: Float -> [Polygon] -> ([Polygon],[Polygon])
      sliceY y = slice ((y >).snd) interpolateY
          where interpolateY (x1,y1) (x2,y2) = (x1+(x2-x1)*(y-y1)/(y2-y1),y)

  let (left_side,right_side) = sliceX 200 triangles

  writeFile "tut3.svg" $ writePolygons $ (red left_side) ++ (blue right_side)

  let boundingRect :: [Polygon] -> (Float,Float,Float,Float)
      boundingRect p = (minimum xs,minimum ys,maximum xs,maximum ys)
          where xs = map fst $ concat p
                ys = map snd $ concat p

  let halveTriangles :: Int -> [Polygon] -> ([Polygon],[Polygon])
      halveTriangles n p = let (l,t,r,b) = boundingRect p
                               f         = fromIntegral n
                               h         = fromIntegral $ div n 2
                           in if r-l > b-t
                              then sliceX ((r*h+l*(f-h))/f) p
                              else sliceY ((b*h+t*(f-h))/f) p

  let distance :: Point -> Point -> Float
      distance p1 p2 = sqrt (deltax*deltax+deltay*deltay)
          where deltax = (fst p1)-(fst p2)
                deltay = (snd p1)-(snd p2)

  let area :: Polygon -> Float
      area [a,b,c] = let x = distance a b
                         y = distance b c
                         z = distance c a
                         s = (x+y+z)/2
                     in sqrt (s*(s-x)*(s-y)*(s-z))

  let allocatePeople :: Int -> [Polygon] -> [[Polygon]]
      allocatePeople 0 t = []
      allocatePeople 1 t = [t]
      allocatePeople n t = let (t1,t2) = halveTriangles n t
                               a1      = sum $ map area t1
                               a2      = sum $ map area t2
                               f = round $ (fromIntegral n)*a1/(a1+a2)
                           in (allocatePeople f t1)++(allocatePeople (n-f) t2)

  let lots = allocatePeople (length people) triangles

  writeFile "tut4.svg" $ writePolygons $ concat $ zipWith ($) (cycle rainbow) lots

  let findLotCenter :: [Polygon] -> Point
      findLotCenter p = let (l,t,r,b) = boundingRect p
                            m@(x,y)   = ((r+l)/2,(b+t)/2)
                            (lh,rh)   = sliceX x p
                            (th,bh)   = sliceY y $ lh ++ rh
                            centerOrder p1 p2 = compare (distance p1 m) (distance p2 m)
                        in minimumBy (comparing $ distance m) $ concat $ th ++ bh

  let makeDot :: Point -> Polygon
      makeDot (x,y) = [(x-2,y-2),(x+2,y-2),(x+2,y+2),(x-2,y+2)]

  let centers = map findLotCenter lots

  let spots = blue $ map makeDot centers

  writeFile "tut5.svg" $ writePolygons $ (green park) ++ spots

  let shortestLinks :: Int -> [Link] -> [Link]
      shortestLinks n = (take n).(sortBy $ comparing linkLength)
       where linkLength [a,b] = distance a b

  let sittingNeighbors :: Int -> [Point] -> [Link]
      sittingNeighbors n p = nub $ shortestLinks (n * (length p)) [[a,b] | a <- p, b <- p, a /= b]

  let sitting = sittingNeighbors 4 centers

  writeFile "tut6.svg" $ writePolygons $ (green park) ++ spots ++ (red sitting)

  let walkingNeighbors :: Int -> [Point] -> [Link]
      walkingNeighbors n l = nub $ concatMap myNeighbors l
          where myNeighbors :: Point -> [Link]
                myNeighbors p = shortestLinks n [sort [p,c] | c <- l, p /= c]

  let walking = walkingNeighbors 4 centers

  writeFile "tut7.svg" $ writePolygons $ (green park) ++ spots ++ (red walking)

  let starting_placement = zip centers people

  let mismatches :: Person -> Person -> Int
      mismatches a b = length $ filter (uncurry (/=)) $ zip a b

  let similarityColor :: Person -> Person -> Color
      similarityColor p1 p2 = let m = mismatches p1 p2
                                  h = div (length p1) 2
                                  d = 30 * (abs (h - m))
                                  b = max 0 (255-d)
                                  o = min d 255
                              in if m < h
                                 then (0,o,b)
                                 else (o,0,b)

  let findPerson :: Placement -> Point -> Person
      findPerson a p | Just (_,e) <- find ((== p).fst) a = e

  let similarityLine :: Placement -> Link -> (Color,Polygon)
      similarityLine l [p1,p2] = (similarityColor (findPerson l p1) (findPerson l p2),[p1,p2])

  writeFile "tut8.svg" $ writePolygons $ map (similarityLine starting_placement) sitting

  let picnicEnergy :: [Link] -> EnergyFunction Placement
      picnicEnergy l a = sum $ map linkEnergy l
          where linkEnergy :: Link -> Int
                linkEnergy [p1,p2] = mismatches (findPerson a p1) (findPerson a p2)

  let picnicMotion :: [Link] -> MotionFunction Placement
      picnicMotion l r a = let (n,r2) = randomR (0,(length l)-1) r
                               [p1,p2] = l!!n
                           in (r2,(p1,findPerson a p2):(p2,findPerson a p1):(filter (not.((flip elem) [p1,p2]).fst) a))

  let picnicTemperature :: TemperatureFunction
      picnicTemperature m c = 50.0 * (exp (0.0 - (5.0 * ((fromIntegral c) / (fromIntegral m)))))

  let picnicTransitionalProbability :: TransitionProbabilityFunction
      picnicTransitionalProbability e1 e2 t = exp ((fromIntegral (e1 - e2)) / t)

  let annealing_time = 500

  putStr "starting energy: "
  print $ picnicEnergy sitting starting_placement

  putStr "starting temperature: "
  print $ picnicTemperature annealing_time annealing_time

  let anneal_tick :: MotionFunction a -> TransitionProbabilityFunction -> EnergyFunction a -> Float -> (StdGen,a) -> (StdGen,a)
      anneal_tick mf tpf ef t (r,p) = let (r2,p2) = mf r p
                                          (n ,r3) = random r2
                                      in (r3,
                                          if n < tpf (ef p) (ef p2) t
                                          then p2
                                          else p)

  let anneal :: EnergyFunction a -> MotionFunction a -> TransitionProbabilityFunction -> TemperatureFunction -> Int -> StdGen -> a -> a
      anneal ef mf tpf tf m r s = snd $ foldl' (flip (anneal_tick mf tpf ef)) (r,s) (map (tf m) [0..m])

  random_generator <- getStdGen

  putStr "starting annealing... "
  putStr "number of annealing steps: "
  print annealing_time

  let ideal_placement = anneal
                        (picnicEnergy sitting)
                        (picnicMotion walking)
                        picnicTransitionalProbability
                        picnicTemperature
                        annealing_time
                        random_generator
                        starting_placement

  writeFile "tut9.svg" $ writePolygons $ map (similarityLine ideal_placement) sitting

  putStr "Done!\nfinal energy: "
  print $ picnicEnergy sitting ideal_placement
  putStr "final temperature: "
  print $ picnicTemperature 0 annealing_time

