module SVG (
  colorize
, readPoint
, readPolygon
, readPolygons
, writePoint
, writePolygon
, writePolygons
) where

import Polygon
import Text.Regex (matchRegex, mkRegex, splitRegex)
import Color

writePoint :: Point -> String
writePoint (x,y) = show x ++ "," ++ show y ++ " "

writePolygon :: (Color,Polygon) -> String
writePolygon ((r,g,b),p) = "<polygon points=\"" ++ concatMap writePoint p ++ "\" style=\"fill:#cccccc;stroke:rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ");stroke-width:2\"/>"

writePolygons :: [(Color,Polygon)] -> String
writePolygons p = svgHeader ++ concatMap writePolygon p ++ svgFooter
  where
    svgHeader = "<svg xmlns=\"http://www.w3.org/2000/svg\">"
    svgFooter = "</svg>"

colorize :: Color -> [Polygon] -> [(Color,Polygon)]
colorize = zip.repeat

readPoint :: String -> Point
readPoint s
  | Just [x,y] <- matchRegex (mkRegex "([0-9.]+),([0-9.]+)") s = (read x, read y)
  | otherwise = undefined -- exhausting patterns

readPolygon :: String -> Polygon
readPolygon = map readPoint . splitRegex (mkRegex " L ")

readPolygons :: String -> [Polygon]
readPolygons = map readPolygon . tail . splitRegex (mkRegex "<path")
