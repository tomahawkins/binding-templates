{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}


module Main (main) where


import Data.Text (Text, pack, unpack)
import qualified Data.Text as T


-- | Generate binding templates for pivot, spx, and shift bindings over a range of BSLs.
main :: IO ()
main = flip mapM_ [280 .. 340] $ \ bsl -> do
  writeFile ("pivot/pivot_bsl_" <> show bsl <> ".svg") $ render pivot bsl
  writeFile ("spx/spx_bsl_" <> show bsl <> ".svg") $ render spx bsl
  writeFile ("shift/shift_bsl_" <> show bsl <> ".svg") $ render shift bsl

  where

  render :: Bindings -> Int -> String
  render binding bsl = unpack $ svg $ template binding $ fromIntegral bsl


-- | A binding spec is a list of holes given a BSL.
--   Holes are X-Y coordinate with the origin on the midsole line in the center of the ski.
--   Positive Y towards the tip, negative Y towards the tail.
newtype Bindings = Bindings (Double -> [(Double, Double)])


-- | Bindings is an instance of Semigroup to append specs together.
instance Semigroup Bindings where
  Bindings a <> Bindings b = Bindings $ \ bsl -> a bsl <> b bsl


-- | Look Pivot bindings spec.
pivot :: Bindings
pivot = lookToe <> Bindings (\ bsl -> symetric
  [ (21 / 2, -(bsl / 2 - 82))
  , (29 / 2, -(bsl / 2 - 82 + 32))
  ])


-- | Look SPX bindings spec.
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
  [ (0, bsl / 2 - 20 + 65) ] <>
  symetric
    [ (40 / 2, bsl / 2 - 20)
    , (30 / 2, bsl / 2 - 20 - 70)
    ] <>
  -- Heel.
  symetric
    [ (36 / 2, -(bsl / 2 - 15))
    , (36 / 2, -(bsl / 2 - 15 + 68))
    ]


-- | Helper for when holes are symetric about the Y axis.
symetric :: [(Double, Double)] -> [(Double, Double)]
symetric = concatMap $ \ (x, y) -> [(x, y), (-x, y)]


-- | Generate a template from a bindings spec and BSL.
template :: Bindings -> Double -> Elements
template (Bindings holes) bsl = mconcat $
  [ centerLines
  , text (60, base1 - 30) $ "BSL: " <> showT (round bsl :: Int) <> " mm"
  , text (60, base2 + 30) $ "BSL: " <> showT (round bsl :: Int) <> " mm"
  ] <> map hole (holes bsl)

  where

  hole :: (Double, Double) -> Elements
  hole (x, y)
    | y >= 0    = target (pageCenter1 + x, base1 - y) 
               <> target (pageCenter2 + x, base1 - y) 
    | otherwise = target (pageCenter1 + x, base2 - y)
               <> target (pageCenter2 + x, base2 - y)


-- | Draws X and Y centerling lines for the template,
--   which align with the mid sole mark and the mid line on the skis.
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


-- | Page dimensions and parameters.

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


-- | Draws a target: crosshair with a circle.
target :: (Double, Double) -> Elements
target (x, y) = circle (x, y) 2.5 <> crosshairs (x, y)


-- | Draws a crosshair.
crosshairs :: (Double, Double) -> Elements
crosshairs (x, y) = mconcat
  [ line (x - 5, y) (x + 5, y)
  , line (x, y - 5) (x, y + 5)
  ]


-- | SVG support.


-- | A collection of SVG elements.
newtype Elements = Elements [Text] deriving (Semigroup, Monoid)


-- | Converts Elements to an SVG file.
svg :: Elements -> Text
svg (Elements elements) = T.unlines $
  [ "<?xml version=\"1.0\"?>"
  , "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
  , "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
  , tag True "svg"
    [ ("xmlns", "http://www.w3.org/2000/svg")
    , ("version", "1.2")
    , ("width", showT pageWidth <> "mm")
    , ("height", showT pageHeight <> "mm")
    , ("viewBox", "0 0 " <> showT pageWidth <> " " <> showT pageHeight)
    ]
  ] <>
  map ("  " <>) elements <>
  [ "</svg>"
  ]


-- | Draws a line between two points.
line :: (Double, Double) -> (Double, Double) -> Elements
line (x1, y1) (x2, y2) = Elements
  [ tag False "line"
    [ ("x1", showT x1)
    , ("y1", showT y1)
    , ("x2", showT x2)
    , ("y2", showT y2)
    , ("style", "stroke:#000000;stroke-width:0.1;")
    ]
  ]


-- | Draws a circle given a center point and a radius.
circle :: (Double, Double) -> Double -> Elements
circle (cx, cy) r = Elements
  [ tag False "circle"
    [ ("cx", showT cx)
    , ("cy", showT cy)
    , ("r", showT r)
    , ("style", "fill:none;stroke:#000000;stroke-width:0.1;")
    ]
  ]


-- | Draws text at a point.
text :: (Double, Double) -> Text -> Elements
text (x, y) msg = Elements
  [ tag True "text"
    [ ("x", showT x)
    , ("y", showT y)
    , ("font-family", "Arial")
    , ("fill", "black")
    , ("font-size", "6")
    ] <> msg <> "</text>"
  ]


-- | Formats an XML tag.
tag :: Bool -> Text -> [(Text, Text)] -> Text
tag open element attributes =
  "<" <> element <>
  mconcat [ " " <> name <> "=\"" <> value <> "\"" | (name, value) <- attributes ] <>
  (if open then " >" else " />")


-- | Show for Text.
showT :: Show a => a -> Text
showT = pack . show
