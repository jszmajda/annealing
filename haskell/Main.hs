import qualified SimulatedAnnealing as SA
import qualified Polygon as PG
import qualified Park
import qualified SVG
import System.Random (getStdGen)

annealTime :: Int
annealTime = 5000
-- annealTime = 5

data PicnicEnv = PicnicEnv {
  pePeople  :: [Park.Person]
, peCenters :: [PG.Point]
, peSitting :: [Park.Link]
, peWalking :: [Park.Link]
}

buildPicnicEnv :: IO PicnicEnv
buildPicnicEnv = do
  people_text <- readFile "people.txt"
  park_data   <- readFile "park.svg"

  let people  = read people_text
  let park    = SVG.readPolygons park_data
  let cnts    = PG.centers park (length people)
  let sitting = Park.sittingNeighbors 2 cnts
  let walking = Park.walkingNeighbors 4 cnts

  return $ PicnicEnv people cnts sitting walking

buildAnnealEnv :: PicnicEnv -> SA.AnnealEnv Park.Placement
buildAnnealEnv pEnv = SA.AnnealEnv
                        (Park.picnicEnergy (peSitting pEnv))
                        Park.picnicTemperature
                        Park.picnicProbability
                        (Park.picnicMutation (peWalking pEnv))

showEnv :: PicnicEnv -> Park.Placement -> Int -> IO ()
showEnv pEnv placement time = do
  putStrLn $ "energy: " ++ show (Park.picnicEnergy (peSitting pEnv) placement)
  putStrLn $ "temperature: " ++ show (Park.picnicTemperature time annealTime)

main :: IO ()
main = do
  pEnv      <- buildPicnicEnv
  randomGen <- getStdGen

  let startingPlacement = zip (peCenters pEnv) (pePeople pEnv)
  let annealEnv = buildAnnealEnv pEnv

  putStrLn $ "Number of people coming: "   ++ show (length (pePeople pEnv))
  putStrLn $ "number of annealing steps: " ++ show annealTime
  showEnv pEnv startingPlacement annealTime

  let finalPlacement = SA.anneal annealEnv annealTime randomGen startingPlacement

  writeFile "final.svg" $ SVG.writePolygons $ map (Park.similarityLine finalPlacement) (peSitting pEnv)

  putStrLn "Done!"
  showEnv pEnv finalPlacement 0
