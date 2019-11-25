{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

main :: IO ()
main = flip mapM_ [240 .. 360 :: Int] $ \ bsl ->
  writeFile ("pivot/pivot_bsl_" ++ show bsl ++ ".svg") $ svg $
    template pivot (fromIntegral bsl)
    -- <> template pivot2 (fromIntegral bsl)

-- | Bindings spec.
data Bindings = Bindings { toePiece, heelPiece :: Piece }

data Piece = Piece
  { innerHoles :: (Double, Double -> Double)
    -- ^ (Width of inner holes, distance from midsole given BSL)
  , outerHoles :: (Double, Double)
    -- ^ (Width of outer holes, distance from inner holes)
  }

-- | Pivot bindings spec.
pivot :: Bindings
pivot = Bindings
  { toePiece = Piece
    { innerHoles = (35, \ bsl -> bsl / 2 - 16.5)   -- Orig: (bsl - 240) / 2 + 106.5
    , outerHoles = (42, 41.5)
    }
  , heelPiece = Piece
    { innerHoles = (21, \ bsl -> (bsl - 240) / 2 + 38)  -- Orig: 38.5
    , outerHoles = (29, 32)
    }
  }

{-
pivot2 :: Bindings
pivot2 = Bindings
  { toePiece = Piece
    { innerHoles = (35, \ bsl -> bsl / 2 - 16.5)   -- Orig: (bsl - 240) / 2 + 106.5
    , outerHoles = (42, 41.5)
    }
  , heelPiece = Piece
    { innerHoles = (21, \ bsl -> bsl / 2 - 82) -- Orig: (bsl - 240) / 2 + 38.5)
    , outerHoles = (29, 32)
    }
  }
-}

-- | Generate a template from a bindings spec and BSL.
template :: Bindings -> Double -> Elements
template bindings bsl = mconcat
  [ centerLines
  , centeringMarks
  , piece (base1 -) (toePiece bindings) bsl
  , piece (base2 +) (heelPiece bindings) bsl
  ]

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

piece :: (Double -> Double) -> Piece -> Double -> Elements
piece offset piece bsl = mconcat
  [ text (40, offset 40) $ "BSL: " ++ show (fromIntegral (round bsl) :: Int) ++ " mm"
  , target (pageCenter - (outerX / 2), offset outerYFromMS)
  , target (pageCenter + (outerX / 2), offset outerYFromMS)
  , target (pageCenter - (innerX / 2), offset innerYFromMS)
  , target (pageCenter + (innerX / 2), offset innerYFromMS)
  ]
  where
  innerYFromMS = innerY bsl
  outerYFromMS = innerYFromMS + outerY

  (innerX, innerY) = innerHoles piece
  (outerX, outerY) = outerHoles piece

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
