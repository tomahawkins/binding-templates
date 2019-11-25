{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

main :: IO ()
main = flip mapM_ [240 .. 360 :: Int] $ \ bsl ->
  writeFile ("pivot/pivot_bsl_" ++ show bsl ++ ".svg") $ svg $
    template pivot (fromIntegral bsl)
    -- <> template pivot2 (fromIntegral bsl)

-- | A binding spec is a list of holes given a BSL.
newtype Bindings = Bindings (Double -> [(Double, Double)])

-- | Pivot bindings spec.
pivot :: Bindings
pivot = Bindings $ \ bsl -> symetric
  [ (35 / 2, bsl / 2 - 16.5)
  , (42 / 2, bsl / 2 - 16.5 + 41.5)
  , (21 / 2, -((bsl - 240) / 2 + 38))
  , (29 / 2, -((bsl - 240) / 2 + 38 + 32))
  ]

symetric :: [(Double, Double)] -> [(Double, Double)]
symetric = concatMap $ \ (x, y) -> [(x, y), (-x, y)]

-- | Generate a template from a bindings spec and BSL.
template :: Bindings -> Double -> Elements
template (Bindings holes) bsl = mconcat $
  [ centerLines
  , centeringMarks
  , text (40, base1 - 40) $ "BSL: " ++ show (fromIntegral (round bsl) :: Int) ++ " mm"
  , text (40, base2 + 40) $ "BSL: " ++ show (fromIntegral (round bsl) :: Int) ++ " mm"
  ] ++ map hole (holes bsl)
  where
  hole (x, y)
    | y >= 0    = target (pageCenter + x, base1 - y) 
    | otherwise = target (pageCenter + x, base2 - y)

centerLines :: Elements
centerLines = mconcat
  [ line (pageCenter, 0) (pageCenter, pageHeight)
  , line (10, base1) (pageWidth - 10, base1)
  , crosshairs (10,  base1)
  , crosshairs (pageWidth - 10, base1)
  , line (10, base2) (pageWidth - 10, base2)
  , crosshairs (10,  base2)
  , crosshairs (pageWidth - 10, base2)
  ]

centeringMarks :: Elements
centeringMarks = mconcat $
  [ mconcat $ map (mark 12) [30, 40 .. 70]
  , mconcat $ map (mark  8) [35, 45 .. 65]
  , mconcat $ map (mark  4) [30 .. 70]
  , mconcat [ line (0, y) (12, y) | y <- map (base1 -) [0, 10 .. 300] ]
  , mconcat [ line (0, y) ( 8, y) | y <- map (base1 -) [5, 15 .. 295] ]
  , mconcat [ line (0, y) ( 4, y) | y <- map (base1 -) [0     .. 300] ]
  , mconcat [ line (0, y) (12, y) | y <- map (base2 +) [0, 10 .. 300] ]
  , mconcat [ line (0, y) ( 8, y) | y <- map (base2 +) [5, 15 .. 295] ]
  , mconcat [ line (0, y) ( 4, y) | y <- map (base2 +) [0     .. 300] ]
  ]
  where
  mark h x = mconcat
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

target :: (Double, Double) -> Elements
target (x, y) = circle (x, y) 2.5 <> crosshairs (x, y)

crosshairs :: (Double, Double) -> Elements
crosshairs (x, y) = mconcat
  [ line (x - 5, y) (x + 5, y)
  , line (x, y - 5) (x, y + 5)
  ]



-- | SVG support.

newtype Elements = Elements [String] deriving (Semigroup, Monoid)

svg :: Elements -> String
svg (Elements elements) = unlines $
  [ "<?xml version=\"1.0\"?>"
  , "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
  , "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
  , tag True "svg"
    [ ("xmlns", "http://www.w3.org/2000/svg")
    , ("version", "1.2")
    , ("width", show pageWidth ++ "mm")
    , ("height", show pageHeight ++ "mm")
    , ("viewBox", "0 0 " ++ show pageWidth ++ " " ++ show pageHeight)
    ]
  ] ++
  map ("  " ++) elements ++
  [ "</svg>"
  ]

line :: (Double, Double) -> (Double, Double) -> Elements
line (x1, y1) (x2, y2) = Elements
  [ tag False "line"
    [ ("x1", show x1)
    , ("y1", show y1)
    , ("x2", show x2)
    , ("y2", show y2)
    , ("style", "stroke:#000000;stroke-width:0.1;")
    ]
  ]

circle :: (Double, Double) -> Double -> Elements
circle (cx, cy) r = Elements
  [ tag False "circle"
    [ ("cx", show cx)
    , ("cy", show cy)
    , ("r", show r)
    , ("style", "fill:none;stroke:#000000;stroke-width:0.1;")
    ]
  ]

text :: (Double, Double) -> String -> Elements
text (x, y) msg = Elements
  [ tag True "text"
    [ ("x", show x)
    , ("y", show y)
    , ("font-family", "Arial")
    , ("fill", "black")
    , ("font-size", "6")
    ] ++ msg ++ "</text>"
  ]

tag :: Bool -> String -> [(String, String)] -> String
tag open element attributes =
  "<" ++ element ++
  concat [ " " ++ name ++ "=" ++ show value | (name, value) <- attributes ] ++
  (if open then " >" else " />")
