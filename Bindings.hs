{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Monad (forM_)

main :: IO ()
main = forM_ [240 .. 360 :: Int] $ \ bsl ->
  writeFile ("pivot/pivot_bsl_" ++ show bsl ++ ".svg") $ svg $ pivot $ fromIntegral bsl

pivot :: Double -> [Element]
pivot bsl = concat
  [ centerLines
  , centeringMarks
  , toePiece bsl
  , heelPiece bsl
  ]

centerLines :: [Element]
centerLines = concat
  [ line (pageCenter, 0) (pageCenter, pageHeight)
  , line (10, base1) (pageWidth - 10, base1)
  , crosshairs (10,  base1)
  , crosshairs (pageWidth - 10, base1)
  , line (10, base2) (pageWidth - 10, base2)
  , crosshairs (10,  base2)
  , crosshairs (pageWidth - 10, base2)
  ]

centeringMarks :: [Element]
centeringMarks = concatMap (mark 12) [30, 40 .. 70] ++
                 concatMap (mark  8) [35, 45 .. 65] ++
                 concatMap (mark  4) [30 .. 70] ++
                 concat [ line (0, y) (12, y) | y <- [30, 40 .. 200] ] ++
                 concat [ line (0, y) ( 8, y) | y <- [35, 45 .. 195] ] ++
                 concat [ line (0, y) ( 4, y) | y <- [30     .. 200] ]
  where
  mark h x = concat
    [ line (pageCenter - x, 0) (pageCenter - x, h)
    , line (pageCenter + x, 0) (pageCenter + x, h)
    , line (pageCenter - x, pageHeight) (pageCenter - x, pageHeight - h)
    , line (pageCenter + x, pageHeight) (pageCenter + x, pageHeight - h)
    ]

pageWidth = 180
pageHeight = 510
pageCenter = pageWidth / 2
base1 = 250
base2 = 270

toePiece :: Double -> [Element]
toePiece bsl = concat
  [ target (pageCenter - (42 / 2), base - 41.5)
  , target (pageCenter + (42 / 2), base - 41.5)
  , target (pageCenter - (35 / 2), base)
  , target (pageCenter + (35 / 2), base)
  ]
  where
  base = base1 - ((bsl - 240) / 2 + 106.5)

heelPiece :: Double -> [Element]
heelPiece bsl = concat
  [ target (pageCenter - (21 / 2), base)
  , target (pageCenter + (21 / 2), base)
  , target (pageCenter - (29 / 2), base + 32)
  , target (pageCenter + (29 / 2), base + 32)
  ]
  where
  base = base2 + ((bsl - 240) / 2 + 38.5)

target :: (Double, Double) -> [Element]
target (x, y) = circle (x, y) 2.5 ++ crosshairs (x, y)

crosshairs :: (Double, Double) -> [Element]
crosshairs (x, y) = concat
  [ line (x - 5, y) (x + 5, y)
  , line (x, y - 5) (x, y + 5)
  ]


svg :: [Element] -> String
svg elements = unlines $
  [ "<?xml version=\"1.0\"?>"
  , "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
  , "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
  , xml True "svg"
    [ ("xmlns", "http://www.w3.org/2000/svg")
    , ("version", "1.2")
    , ("width", show pageWidth ++ "mm")
    , ("height", show pageHeight ++ "mm")
    , ("viewBox", "0 0 " ++ show pageWidth ++ " " ++ show pageHeight)
    ]
  ] ++
  map (("  " ++) . render) elements ++
  [ "</svg>"
  ]

line :: (Double, Double) -> (Double, Double) -> [Element]
line a b = [Line a b]

circle :: (Double, Double) -> Double -> [Element]
circle a b = [Circle a b]

data Element
  = Line (Double, Double) (Double, Double)  -- ^ (x1, y1) (x2, y2)
  | Circle (Double, Double) Double          -- ^ (x, y) r

render :: Element -> String
render = \case
  Line (x1, y1) (x2, y2) -> xml False "line"
    [ ("x1", show x1)
    , ("y1", show y1)
    , ("x2", show x2)
    , ("y2", show y2)
    , ("style", "stroke:#000000;stroke-width:0.1;")
    ]

  Circle (cx, cy) r -> xml False "circle"
    [ ("cx", show cx)
    , ("cy", show cy)
    , ("r", show r)
    , ("style", "fill:none;stroke:#000000;stroke-width:0.1;")
    ]

xml :: Bool -> String -> [(String, String)] -> String
xml open element attributes =
  "<" ++ element ++
  concat [ " " ++ name ++ "=" ++ show value | (name, value) <- attributes ] ++
  (if open then " >" else " />")
