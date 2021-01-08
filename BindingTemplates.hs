{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}


module Main (main) where


import Control.Monad (forM_)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T


-- | Generate binding templates for pivot, spx, and shift bindings over a range of BSLs.
main :: IO ()
main = forM_ [280 .. 340 :: Int] $ \ bsl ->
  forM_ [pivot, spx, shift, jester, tyrolia] $ \ binding@(BindingSpec name _ _) ->
    writeFile (unpack name <> "/" <> unpack name <> "_bsl_" <> show bsl <> ".svg") $
      unpack $ svg $ template binding $ fromIntegral bsl


-- | Binding spec is the name of the binding and the hole locations for the toe and heel piece.
data BindingSpec = BindingSpec Text ToePiece HeelPiece


-- | Spec for the toe piece.  X axis is position of the toe edge of the boot.
newtype ToePiece = ToePiece [Hole]


-- | Spec for the heel piece.  X axis is position of the heel edge of the boot.
newtype HeelPiece = HeelPiece [Hole]


-- | Coordinates for hole.
type Hole = Point


-- | Coordinates for a point in mm.
type Point = (Double, Double)


-- | Look Pivot bindings spec.
pivot :: BindingSpec
pivot = BindingSpec "pivot" lookToe $ HeelPiece $ symetric
  [ (21 / 2, 82)
  , (29 / 2, 82 - 32)
  ]


-- | Look SPX bindings spec.
spx :: BindingSpec
spx = BindingSpec "spx" lookToe $ HeelPiece $ symetric
  [ (42 / 2, 26)
  , (42 / 2, 26 - 105)
  ]


-- | Spec for the common LOOK toe piece.
lookToe :: ToePiece
lookToe = ToePiece $ symetric
  [ (35 / 2, - 16.5)
  , (42 / 2, - 16.5 + 41.5)
  ]


-- | Salomon Shift bindings spec.
shift :: BindingSpec
shift = BindingSpec "shift" toePiece heelPiece

  where

  toePiece = ToePiece $ (0, - 20 + 65) : symetric
    [ (40 / 2, - 20)
    , (30 / 2, - 20 - 70)
    ]
  
  heelPiece = HeelPiece $ symetric
    [ (36 / 2, 15)
    , (36 / 2, 15 - 68)
    ]


-- | Marker Jester, Griffon, and Squire binding spec.
jester :: BindingSpec
jester = BindingSpec "jester" toePiece heelPiece

  where

  toePiece = ToePiece $ symetric
    [ (36 / 2, 19)
    , (36 / 2, -12)
    ]

  heelPiece = HeelPiece $ symetric
    [ (32 / 2, 22)
    , (32 / 2, -58)
    ]


-- | Tyrolia binding spec.
tyrolia :: BindingSpec
tyrolia = BindingSpec "tyrolia" toePiece heelPiece

  where

  toePiece = ToePiece $ symetric
    [ (20, 45.5)
    , (20, -9.5)
    ]

  heelPiece = HeelPiece $ symetric
    [ (10, 16.5)
    , (21.25, -78.5)
    ]


-- | Helper for when holes are symetric about the Y axis.
symetric :: [(Double, Double)] -> [(Double, Double)]
symetric = concatMap $ \ (x, y) -> [(x, y), (-x, y)]




-- | A drawing is collection of SVG elements.
newtype Drawing = Drawing [Text] deriving (Semigroup, Monoid)


-- | Draws a line between two points.
line :: Point -> Point -> Drawing
line (x1, y1) (x2, y2) = Drawing
  [ tag False "line"
    [ ("x1", showT x1)
    , ("y1", showT y1)
    , ("x2", showT x2)
    , ("y2", showT y2)
    , ("style", "stroke:#000000;stroke-width:0.1;")
    ]
  ]


-- | Draws a circle given a center point and a radius.
circle :: Point -> Double -> Drawing
circle (cx, cy) r = Drawing
  [ tag False "circle"
    [ ("cx", showT cx)
    , ("cy", showT cy)
    , ("r", showT r)
    , ("style", "fill:none;stroke:#000000;stroke-width:0.1;")
    ]
  ]


-- | Draws text at a point.
text :: Point -> Text -> Drawing
text (x, y) msg = Drawing
  [ tag True "text"
    [ ("x", showT x)
    , ("y", showT y)
    , ("font-family", "Arial")
    , ("fill", "black")
    , ("font-size", "6")
    ] <> msg <> "</text>"
  ]


-- | Draws a crosshair.
crosshairs :: Point -> Drawing
crosshairs (x, y) = mconcat
  [ line (x - 5, y) (x + 5, y)
  , line (x, y - 5) (x, y + 5)
  ]


-- | Draws a target: crosshair with a circle.
target :: Point -> Drawing
target (x, y) = circle (x, y) 2.5 <> crosshairs (x, y)




-- | Generate a template from a bindings spec and BSL.
template :: BindingSpec -> Double -> Drawing
template (BindingSpec _name (ToePiece toeHoles) (HeelPiece heelHoles)) bsl = mconcat $
  [ centerLines
  , text (60, base1 - 30) $ "BSL: " <> showT (round bsl :: Int) <> " mm"
  , text (60, base2 + 30) $ "BSL: " <> showT (round bsl :: Int) <> " mm"
  ] <> (toeHole <$> toeHoles) <> (heelHole <$> heelHoles)

  where

  toeHole :: Point -> Drawing
  toeHole (x, y) = hole (x, y + bsl / 2)

  heelHole :: Point -> Drawing
  heelHole (x, y) = hole (x, y - bsl / 2)

  hole :: (Double, Double) -> Drawing
  hole (x, y)
    | y >= 0    = target (pageCenter1 + x, base1 - y) 
               <> target (pageCenter2 + x, base1 - y) 
    | otherwise = target (pageCenter1 + x, base2 - y)
               <> target (pageCenter2 + x, base2 - y)


-- | Draws X and Y centerling lines for the template,
--   which align with the mid sole mark and the mid line on the skis.
centerLines :: Drawing
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




-- | Converts Drawing to an SVG file.
svg :: Drawing -> Text
svg (Drawing elements) = T.unlines $
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


-- | Formats an XML tag.
tag :: Bool -> Text -> [(Text, Text)] -> Text
tag open element attributes =
  "<" <> element <>
  mconcat [ " " <> name <> "=\"" <> value <> "\"" | (name, value) <- attributes ] <>
  (if open then " >" else " />")


-- | Show for Text.
showT :: Show a => a -> Text
showT = pack . show
