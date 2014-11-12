import qualified SimulatedAnnealing as SA
import qualified Polygon as PG
import qualified Park
import qualified SVG
import System.Random (getStdGen)
import Data.List
import Text.Printf
import Data.Function (on)

annealTime :: Int
-- annealTime = 100
annealTime = 2

data PicnicEnv = PicnicEnv {
  pePeople  :: [Park.Person]
, peCenters :: [PG.Point]
, peSitting :: [Park.Link]
, peWalking :: [Park.Link]
}

buildPicnicEnv :: IO PicnicEnv
buildPicnicEnv = do
  people_text <- readFile "../pl2.txt"
  park_data   <- readFile "../pk2.svg"

  let people  = read people_text
  let park    = SVG.readPolygons park_data
  let cnts    = PG.centers park (length people)
  let sitting = Park.sittingNeighbors 2 cnts
  let walking = Park.walkingNeighbors 4 cnts
  -- putStrLn $ "p0: " ++ show (sum (head people))
  -- putStrLn $ "p1: " ++ show (sum (people !! 1))

  return $ PicnicEnv people cnts sitting walking

buildAnnealEnv :: PicnicEnv -> SA.AnnealEnv Park.Placement
buildAnnealEnv pEnv =
  SA.AnnealEnv
    (Park.picnicEnergy (peSitting pEnv))
    Park.picnicTemperature
    Park.picnicTransitionalProbability
    (Park.picnicMotion (peWalking pEnv))

showEnv :: PicnicEnv -> Park.Placement -> Int -> IO ()
showEnv pEnv placement time = do
  putStrLn $ "energy: " ++ show (Park.picnicEnergy (peSitting pEnv) placement)
  putStrLn $ "temperature: " ++ show (Park.picnicTemperature time annealTime)

outputCenters :: PicnicEnv -> IO ()
outputCenters e = writeFile "../hcnts.txt" centers
  where
    centers = intercalate "\n" $ map sh cnts
    cnts = sortBy (compare `on` ucp) $ peCenters e
    ucp = uncurry (+)
    sh c = "(" ++ shf (fst c) ++ "," ++ shf (snd c) ++ ")"
    shf = printf "%0.3f"

main :: IO ()
main = do
  pEnv      <- buildPicnicEnv
  randomGen <- getStdGen

  outputCenters pEnv

  let startingPlacement = zip (peCenters pEnv) (pePeople pEnv)
  let annealEnv = buildAnnealEnv pEnv

  putStrLn $ "Number of people coming: "   ++ show (length (pePeople pEnv))
  putStrLn $ "number of annealing steps: " ++ show annealTime
  showEnv pEnv startingPlacement annealTime

  let finalPlacement = SA.anneal annealEnv annealTime randomGen startingPlacement

  putStrLn $ "sitting links: " ++ show ( length (peSitting pEnv))

  writeFile "final.svg" $ SVG.writePolygons $ map (Park.similarityLine finalPlacement) (peSitting pEnv)

  putStrLn "Done!"
  showEnv pEnv finalPlacement 0
