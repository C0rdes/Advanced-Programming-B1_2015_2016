module Curves where

import Text.Printf

data Point = Point (Double, Double) deriving (Show)

instance Eq Point where
    x == y = (abs(pointX x - pointX y) < 0.01) && (abs(pointY x - pointY y) < 0.01)

point :: (Double, Double) -> Point
point (a, b) = Point (a, b)

pointX :: Point -> Double
pointX (Point p) = fst p

pointY :: Point -> Double
pointY (Point p) = snd p

data Curve = Curve (Point, [Point]) deriving (Show)

instance Eq Curve where
    x == y = (gets0 x == gets0 y) && (getPoints x == getPoints y)

gets0 :: Curve -> Point
gets0 (Curve c) = fst c

getPoints :: Curve -> [Point]
getPoints (Curve c) = snd c

curve :: Point -> [Point] -> Curve
curve p px = Curve (p, px)

connect :: Curve -> Curve -> Curve
connect (Curve c1) (Curve c2) = Curve (fst c1, snd c1 ++ uncurry (:) c2)

rotate :: Curve -> Double -> Curve
rotate (Curve c) i = let rads = i * pi/180
                         points = snd c
                         p0x = pointX (fst c)
                         p0y = pointY (fst c)
                         res = map (\x -> Point(pointX x *cos rads - pointY x*sin rads, pointX x*sin rads + pointY x*cos rads)) points
                     in Curve (Point (p0x*cos rads-p0y*sin rads, p0x*sin rads+p0y*cos rads), res)

translate :: Curve -> Point -> Curve
translate (Curve c) p = let delta_x = pointX p - pointX (fst c)
                            delta_y = pointY p - pointY (fst c)
                            res = map (\x -> Point(pointX x + delta_x, pointY x  + delta_y)) (snd c)
                        in Curve (p, res)

data Line = Vertical Double | Horizontal Double

reflect :: Curve -> Line -> Curve
reflect (Curve c) (Vertical d) = let points = map (\x -> Point(pointX x - 2*(pointX x - d), pointY x)) (snd c)
                                     xval = pointX(fst c)
                                 in Curve (Point(xval - 2*(xval - d), pointY(fst c)), points)
reflect (Curve c) (Horizontal d) = let points = map (\x -> Point(pointX x, pointY x - 2*(pointY x - d))) (snd c)
                                       yval = pointY(fst c)
                                   in Curve (Point(pointX(fst c), yval - 2*(yval - d)), points)

bbox :: Curve -> (Point, Point)
bbox (Curve c) = let xvals = pointX(fst c) : map pointX (snd c)
                     yvals = pointY(fst c) : map pointY (snd c)
                 in (point (minimum xvals, minimum yvals), point(maximum xvals, maximum yvals))
                 
width :: Curve -> Double
width (Curve c) = let xmin = pointX $ fst $ bbox (Curve c)
                      xmax = pointX $ snd $ bbox (Curve c)
                  in abs(xmax - xmin)
                 
height :: Curve -> Double
height (Curve c) = let ymin = pointY $ fst $ bbox (Curve c)
                       ymax = pointY $ snd $ bbox (Curve c)
                   in abs(ymax - ymin)                 
                 
toList :: Curve -> [Point]
toList (Curve c) = uncurry (:) c               

toSVG :: Curve -> String
toSVG (Curve c) = let pointList = toList (Curve c)
                      intro = "<svg xmlns=\"http://www.w3.org/2000/svg\"\n      width=\"" ++ printf "%f" (width (Curve c)) ++ "px\" height=\"" ++ printf "%f" (width (Curve c)) ++ "px\" version=\"1.1\">\n<g>"
                  in intro ++ toSVGrecursion pointList ++ "</g>\n</svg>"
                 
toSVGrecursion :: [Point] -> String
toSVGrecursion [] = ""
toSVGrecursion [_] = "" 
toSVGrecursion (x:y:xs) = "<line style=\"stroke-width: 2px; stroke: black; fill:white\"\n            "
                             ++ "x1=\"" ++ printf "%f" (pointX x) ++ "\" x2=\"" ++ printf "%f" (pointX y) 
                          ++ "\" y1=\"" ++ printf "%f" (pointY x) ++ "\" y2=\"" ++ printf "%f" (pointY y) ++ "\" />\n" ++ toSVGrecursion (y:xs) 
                 
toFile :: Curve -> FilePath -> IO ()
toFile (Curve c) path = writeFile path (toSVG (Curve c))                 
                 
hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c $ Vertical 0

          c0 = ch `rotate` (-90) `translate` point (w+p+w, h+p+h)
          c1 = c `translate` point (w+p+w, h)
          c2 = c
          c3 = ch `rotate` 90 `translate` point (0, h+p)                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 