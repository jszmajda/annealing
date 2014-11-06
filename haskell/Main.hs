import qualified SimulatedAnnealing as SA
import qualified Polygon as PG
import qualified Park
import qualified SVG
import System.Random (getStdGen)

annealTime :: Int
annealTime = 100

-- data PicnicEnv = PicnicEnv {
--   people  :: [Park.Person]
-- , park    :: [Polygon]
-- , centers :: [Point]
-- , sitting :: [Link]
-- , walking :: [Link]
-- }

main :: IO ()
main = do
  people_text <- readFile "people.txt"
  park_data   <- readFile "park.svg"

  let people  = read people_text
  let park    = SVG.readPolygons park_data
  let cnts    = PG.centers park people
  let sitting = Park.sittingNeighbors 4 cnts
  let walking = Park.walkingNeighbors 4 cnts

  let startingPlacement = zip cnts people

  putStrLn $ "Number of people coming: " ++ show (length people)
  putStrLn $ "number of annealing steps: " ++ show annealTime
  putStrLn $ "starting energy: "         ++ show (Park.picnicEnergy sitting startingPlacement)
  putStrLn $ "starting temperature: "    ++ show (Park.picnicTemperature annealTime annealTime)

  let aeEnv = SA.AnnealEnv (Park.picnicEnergy sitting)
                           Park.picnicTemperature
                           Park.picnicTransitionalProbability
                           (Park.picnicMotion walking)

  randomGen <- getStdGen

  let finalPlacement = SA.anneal
                         aeEnv
                         annealTime
                         randomGen
                         startingPlacement

  writeFile "final.svg" $ SVG.writePolygons $ map (Park.similarityLine finalPlacement) sitting

  putStrLn "Done!"
  putStrLn $ "final energy: " ++ show (Park.picnicEnergy sitting finalPlacement)
  putStrLn $ "final temperature: " ++ show (Park.picnicTemperature 0 annealTime)
