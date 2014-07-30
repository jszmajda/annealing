module SVG (writePoint, writePolygon, writePolygons, colorize, readPoint, readPolygon, readPolygons) where

import PicnicTypes
import Text.Regex

writePoint :: Point -> String
writePoint (x,y) = (show x)++","++(show y)++" "

writePolygon :: (Color,Polygon) -> String
writePolygon ((r,g,b),p) = "<polygon points=\""++(concatMap writePoint p)++"\" style=\"fill:#cccccc;stroke:rgb("++(show r)++","++(show g)++","++(show b)++");stroke-width:2\"/>"

writePolygons :: [(Color,Polygon)] -> String
writePolygons p = "<svg xmlns=\"http://www.w3.org/2000/svg\">"++(concatMap writePolygon p)++"</svg>"

colorize :: Color -> [Polygon] -> [(Color,Polygon)]
colorize = zip.repeat


readPoint :: String -> Point
readPoint s | Just [x,y] <- matchRegex (mkRegex "([0-9.]+),([0-9.]+)") s = (read x,read y)

readPolygon :: String -> Polygon
readPolygon = (map readPoint).(splitRegex $ mkRegex " L ")

readPolygons :: String -> [Polygon]
readPolygons = (map readPolygon).tail.(splitRegex $ mkRegex "<path")
