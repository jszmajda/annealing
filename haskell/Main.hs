import qualified SimulatedAnnealing as SA
import qualified Polygon as PG
import qualified Park
import qualified SVG
import System.Random (getStdGen)


main :: IO ()
main = do
  putStrLn "------------"
  people_text <- readFile "people.txt"

  let people :: [Park.Person]
      people = read people_text

  putStr "Number of people coming: "
  print (length people)

  let rainbow@[red,green,blue,yellow,purple,teal] = map SVG.colorize [(255,0,0),(0,255,0),(0,0,255),(255,255,0),(255,0,255),(0,255,255)]

  park_data <- readFile "park.svg"
  let park = SVG.readPolygons park_data

  let cnts = PG.centers park people

  let sitting = Park.sittingNeighbors 4 cnts
  let walking = Park.walkingNeighbors 4 cnts

  let starting_placement = zip cnts people

  -- TODO put back to 500
  let annealing_time = 2

  putStr "starting energy: "
  print $ Park.picnicEnergy sitting starting_placement

  putStr "starting temperature: "
  print $ Park.picnicTemperature annealing_time annealing_time

  random_generator <- getStdGen
  putStr "starting annealing... "
  putStr "number of annealing steps: "
  print annealing_time

  let aeEnv = SA.AnnealEnv (Park.picnicEnergy sitting)
                           Park.picnicTemperature
                           Park.picnicTransitionalProbability
                           (Park.picnicMotion walking)

  let ideal_placement = SA.anneal aeEnv
                        annealing_time
                        random_generator
                        starting_placement

  writeFile "tut9.svg" $ SVG.writePolygons $ map (Park.similarityLine ideal_placement) sitting
  putStr "Done!\nfinal energy: "
  print $ Park.picnicEnergy sitting ideal_placement
  putStr "final temperature: "
  print $ Park.picnicTemperature 0 annealing_time
