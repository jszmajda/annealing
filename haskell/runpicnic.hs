import SimulatedAnnealing
import PicnicTypes
import Picnic
import SVG

main :: IO ()
main = do
  putStr "Hello World! Let's have a picnic! \n"

  people_text <- readFile "people.txt"

  let people :: [Person]
      people = read people_text

  putStr "Number of people coming: "
  print (length people)

  let rainbow@[red,green,blue,yellow,purple,teal] = map colorize [(255,0,0),(0,255,0),(0,0,255),(255,255,0),(255,0,255),(0,255,255)]

  park_data <- readFile "park.svg"
  let park = readPolygons park_data

  writeFile "tut1.svg" $ writePolygons (green park)
