import qualified SimulatedAnnealing as SA
import qualified Polygon as PG
import qualified Park
import qualified SVG
import System.Random (getStdGen)

annealTime :: Int
annealTime = 100

data PicnicEnv = PicnicEnv {
  pePeople  :: [Park.Person]
, pePark    :: [PG.Polygon]
, peCenters :: [PG.Point]
, peSitting :: [Park.Link]
, peWalking :: [Park.Link]
}

buildPEnv :: IO PicnicEnv
buildPEnv = do
  people_text <- readFile "people.txt"
  park_data   <- readFile "park.svg"

  let people  = read people_text
  let park    = SVG.readPolygons park_data
  let cnts    = PG.centers park people
  let sitting = Park.sittingNeighbors 4 cnts
  let walking = Park.walkingNeighbors 4 cnts

  return $ PicnicEnv people park cnts sitting walking

showBeginningEnv :: PicnicEnv -> Park.Placement -> IO ()
showBeginningEnv pEnv startingPlacement = do
  putStrLn $ "Number of people coming: "   ++ show (length (pePeople pEnv))
  putStrLn $ "number of annealing steps: " ++ show annealTime
  putStrLn $ "starting energy: "           ++ show (Park.picnicEnergy (peSitting pEnv) startingPlacement)
  putStrLn $ "starting temperature: "      ++ show (Park.picnicTemperature annealTime annealTime)

main :: IO ()
main = do
  pEnv <- buildPEnv

  let startingPlacement = zip (peCenters pEnv) (pePeople pEnv)

  showBeginningEnv pEnv startingPlacement

  let aeEnv = SA.AnnealEnv (Park.picnicEnergy (peSitting pEnv))
                           Park.picnicTemperature
                           Park.picnicTransitionalProbability
                           (Park.picnicMotion (peWalking pEnv))

  randomGen <- getStdGen

  let finalPlacement = SA.anneal
                         aeEnv
                         annealTime
                         randomGen
                         startingPlacement

  writeFile "final.svg" $ SVG.writePolygons $ map (Park.similarityLine finalPlacement) (peSitting pEnv)

  putStrLn "Done!"
  putStrLn $ "final energy: " ++ show (Park.picnicEnergy (peSitting pEnv) finalPlacement)
  putStrLn $ "final temperature: " ++ show (Park.picnicTemperature 0 annealTime)
