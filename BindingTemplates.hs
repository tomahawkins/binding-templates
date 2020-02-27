{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

main :: IO ()
main = flip mapM_ bslRange $ \ bsl -> do
  writeFile ("pivot/pivot_bsl_" ++ show bsl ++ ".svg") $ svg $ template pivot (fromIntegral bsl)
  writeFile ("spx/spx_bsl_" ++ show bsl ++ ".svg") $ svg $ template spx (fromIntegral bsl)
  writeFile ("shift/shift_bsl_" ++ show bsl ++ ".svg") $ svg $ template shift (fromIntegral bsl)

bslRange :: [Int]
bslRange = [280 .. 340]

-- | A binding spec is a list of holes given a BSL.
newtype Bindings = Bindings (Double -> [(Double, Double)])

instance Semigroup Bindings where
  Bindings a <> Bindings b = Bindings $ \ bsl -> a bsl <> b bsl

-- | Pivot bindings spec.
pivot :: Bindings
pivot = lookToe <> Bindings (\ bsl -> symetric
  [ (21 / 2, -(bsl / 2 - 82))
  , (29 / 2, -(bsl / 2 - 82 + 32))
  ])

-- | SPX bindings spec.
spx :: Bindings
spx = lookToe <> Bindings (\ bsl -> symetric
  [ (42 / 2, -(bsl / 2 - 26))
  , (42 / 2, -(bsl / 2 - 26 + 105))
  ])

-- | Spec for the common LOOK toe piece.
lookToe :: Bindings
lookToe = Bindings $ \ bsl -> symetric
  [ (35 / 2, bsl / 2 - 16.5)
  , (42 / 2, bsl / 2 - 16.5 + 41.5)
  ]

-- | Salomon Shift bindings spec.
shift :: Bindings
shift = Bindings $ \ bsl ->
  -- Toe.
  [ (0,      bsl / 2 - 20 + 65) ] <> symetric
  [ (40 / 2, bsl / 2 - 20)
  , (30 / 2, bsl / 2 - 20 - 70)
  ] <>
  -- Heel.
  symetric
  [ (36 / 2, -(bsl / 2 - 15))
  , (36 / 2, -(bsl / 2 - 15 + 68))
  ]

symetric :: [(Double, Double)] -> [(Double, Double)]
symetric = concatMap $ \ (x, y) -> [(x, y), (-x, y)]

-- | Generate a template from a bindings spec and BSL.
template :: Bindings -> Double -> Elements
template (Bindings holes) bsl = mconcat $
  [ centerLines
  , text (60, base1 - 30) $ "BSL: " ++ show (round bsl :: Int) ++ " mm"
  , text (60, base2 + 30) $ "BSL: " ++ show (round bsl :: Int) ++ " mm"
  ] ++ map hole (holes bsl)
  where
  hole (x, y)
    | y >= 0    = target (pageCenter1 + x, base1 - y) 
               <> target (pageCenter2 + x, base1 - y) 
    | otherwise = target (pageCenter1 + x, base2 - y)
               <> target (pageCenter2 + x, base2 - y)

centerLines :: Elements
centerLines = mconcat
  [ line (pageCenter1, 0) (pageCenter1, pageHeight)
  , line (pageCenter2, 0) (pageCenter2, pageHeight)
  , line (10, base1) (pageWidth - 10, base1)
  , crosshairs (10,  base1)
  , crosshairs (pageWidth - 10, base1)
  , line (10, base2) (pageWidth - 10, base2)
  , crosshairs (10,  base2)
  , crosshairs (pageWidth - 10, base2)
  ]

pageWidth :: Double
pageWidth = 190

pageHeight :: Double
pageHeight = 518

pageCenter :: Double
pageCenter = pageWidth / 2

pageCenter1 :: Double
pageCenter1 = pageCenter / 2

pageCenter2 :: Double
pageCenter2 = pageCenter / 2 + pageCenter

base1 :: Double
base1 = 255

base2 :: Double
base2 = 265

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
