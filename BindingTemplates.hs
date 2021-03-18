{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Main (main) where


import Control.Monad (forM_)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T


-- | Generate binding templates for pivot, spx, and shift bindings over a range of BSLs.
main :: IO ()
main = do
  generateTemplateLibrary
  customExample


-- | An example of a custom placement for a remount.
customExample :: IO ()
customExample = writeFile "custom-example.svg" $ unpack $ svg $ template
  [ PlaceToe  royalToe  334 0       -- Marker Griffons mounted on the line with 334 BSL.
  , PlaceHeel royalHeel 334 0
  , PlaceToe  lookToe   334 (-1.1)  -- Remounted with Look Pivots mounted back 1.1 mm
  , PlaceHeel pivotHeel 334 (-1.1)  -- to maximize toe binding hole separation.
  ]


-- | Generate all templates over a range of BSLs with a mount point of 0.
generateTemplateLibrary :: IO ()
generateTemplateLibrary = do
  forM_ [270 .. 340 :: Int] $ \ bsl ->
    forM_ templateLibrary $ \ (name, toe, heel) ->
      writeFile (unpack name <> "/" <> unpack name <> "_bsl_" <> show bsl <> ".svg") $
        unpack $ svg $ template
          [ PlaceToe toe (fromIntegral bsl) 0
          , PlaceHeel heel (fromIntegral bsl) 0
          ]
  writeFile "r22.svg" $ unpack $ svg $ template [PlacePlate r22 0]


-- | Library of all the templates.
templateLibrary :: [(Text, ToeBinding, HeelBinding)]
templateLibrary = 
  [ ("pivot", lookToe, pivotHeel)
  , ("spx", lookToe, spxHeel)
  , ("rockerace", lookToe, rockeraceHeel)
  , ("royal", royalToe, royalHeel)
  , ("shift", shiftToe, shiftHeel)
  , ("sth2", sth2Toe, sth2Heel)
  , ("warden", wardenToe, sth2Heel)
  , ("tyrolia", tyroliaToe, tyroliaHeel)
  ]


-- | Placement of either a binding or a plate.
data Placement
  = PlaceToe ToeBinding BSL MountPoint
  | PlaceHeel HeelBinding BSL MountPoint
  | PlacePlate Plate MountPoint


-- | Spec for the toe piece.  X axis is position of the toe edge of the boot.
newtype ToeBinding = ToeBinding [Hole]


-- | Spec for the heel piece.  X axis is position of the heel edge of the boot.
newtype HeelBinding = HeelBinding [Hole]


-- | Spec for a binding plate.
newtype Plate = Plate [Hole]


-- | Coordinates for hole.
type Hole = Point


-- | Coordinates for a point in mm.
type Point = (Double, Double)


-- | Boot sole length in mm.
type BSL = Double


-- | Mount point, + in front of the line, - behind the line, in mm.
type MountPoint = Double


-- | Look Pivot heel.
pivotHeel :: HeelBinding
pivotHeel = HeelBinding $ symetric
  [ (21 / 2, 82)
  , (29 / 2, 82 - 32)
  ]


-- | Look SPX heel.
spxHeel :: HeelBinding
spxHeel = HeelBinding $ symetric
  [ (42 / 2, 26)
  , (42 / 2, 26 - 105)
  ]


-- | Look Rockerace heel.
rockeraceHeel :: HeelBinding
rockeraceHeel = HeelBinding $ symetric
  [ (41.5 / 2, 61)
  , (41.5 / 2, 61 - 40)
  ]


-- | The common LOOK toe.
lookToe :: ToeBinding
lookToe = ToeBinding $ symetric
  [ (35 / 2, - 16.5)
  , (42 / 2, - 16.5 + 41.5)
  ]


-- | Look R22 racing plate.
r22 :: Plate
r22 = Plate $ symetric
  [ (12 / 2, 163)
  , (35 / 2, 98)
  , (35 / 2, -52)
  , (35 / 2, -(52 + 120))
  ]


-- | Salomon Shift.
shiftToe :: ToeBinding
shiftToe = ToeBinding $ (0, - 20 + 65) : symetric
    [ (40 / 2, - 20)
    , (30 / 2, - 20 - 70)
    ]

shiftHeel :: HeelBinding
shiftHeel = HeelBinding $ symetric
    [ (36 / 2, 15)
    , (36 / 2, 15 - 68)
    ]


-- | Salomon STH2.
sth2Toe :: ToeBinding
sth2Toe = ToeBinding $ symetric
  [ (42 / 2, - 15 + 30)
  , (40 / 2, - 15)
  ]

sth2Heel :: HeelBinding
sth2Heel = HeelBinding $ symetric
  [ (32 / 2, 28)
  , (32 / 2, 28 - 75)
  ]


-- | Salomon Warden toe.
wardenToe :: ToeBinding
wardenToe = ToeBinding $ symetric
  [ (40 / 2, - 15 + 65)
  , (40 / 2, - 15)
  ]


-- | Marker Royal family (Jester, Griffon, Squire).
royalToe :: ToeBinding
royalToe = ToeBinding $ symetric
    [ (36 / 2, - 12 + 31)
    , (36 / 2, - 12)
    ]

royalHeel :: HeelBinding
royalHeel = HeelBinding $ symetric
    [ (32 / 2, 25)
    , (32 / 2, 25 - 80)
    ]


-- | Tyrolia.
tyroliaToe :: ToeBinding
tyroliaToe = ToeBinding $ symetric
    [ (40 / 2, - 15 + 55)
    , (40 / 2, - 15)
    ]

tyroliaHeel :: HeelBinding
tyroliaHeel = HeelBinding $ symetric
    [ (20   / 2, 17)
    , (42.5 / 2, 17 - 95)
    ]


-- | Helper for when holes are symetric about the Y axis.
symetric :: [Hole] -> [Hole]
symetric = concatMap $ \ (x, y) -> [(x, y), (-x, y)]


-- | Generate a template for a set of binding placements.
template :: [Placement] -> Drawing
template placements = centerLines <> mconcat (placement <$> placements)


-- | Draw the holes for a binding placement.
placement :: Placement -> Drawing
placement = \case

  PlaceToe (ToeBinding holes) bsl mount ->
    bootRefLine mount <>
    bootRefLine (mount + bsl / 2) <>
    mconcat ((hole $ mount + bsl / 2) <$> holes)

  PlaceHeel (HeelBinding holes) bsl mount ->
    bootRefLine mount <>
    bootRefLine (mount - bsl / 2) <>
    mconcat ((hole $ mount - bsl / 2) <$> holes)

  PlacePlate (Plate holes) mount -> mconcat $ hole mount <$> holes


-- | Draw a dashed line for a boot reference line, e.g. mid sole, toe edge, heel edge.
bootRefLine :: MountPoint -> Drawing
bootRefLine mount = dashedLine (leftMargin, base - mount) (rightMargin, base - mount)

  where

  base = if mount >= 0 then base1 else base2


-- | Draw a hole with a y bias.
hole :: Double -> Point -> Drawing
hole bias (x, y) = target (pageCenter1 + x, base - y') <> target (pageCenter2 + x, base - y') 

  where

  base = if y' >= 0 then base1 else base2
  y' = y + bias


-- | Draws X and Y centerling lines for the template,
--   which align with the mid sole mark and the mid line on the skis.
centerLines :: Drawing
centerLines = mconcat $
  -- Center lines.
  [ line (pageCenter1, 0) (pageCenter1, pageHeight)
  , line (pageCenter2, 0) (pageCenter2, pageHeight)
  ] <>

  -- Center mount line.
  [ line (leftMargin, base1) (rightMargin, base1)
  , crosshairs (leftMargin,  base1)
  , crosshairs (rightMargin, base1)
  , line (leftMargin, base2) (rightMargin, base2)
  , crosshairs (leftMargin,  base2)
  , crosshairs (rightMargin, base2)
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

leftMargin :: Double
leftMargin = 10

rightMargin :: Double
rightMargin = pageWidth - 10


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
    , ("stroke", "black")
    , ("stroke-width", "0.1")
    ]
  ]


-- | Dashed line between two points.
dashedLine :: Point -> Point -> Drawing
dashedLine (x1, y1) (x2, y2) = Drawing
  [ tag False "line"
    [ ("x1", showT x1)
    , ("y1", showT y1)
    , ("x2", showT x2)
    , ("y2", showT y2)
    , ("stroke", "black")
    , ("stroke-width", "0.1")
    , ("stroke-dasharray", "4 4")
    ]
  ]


-- | Draws a circle given a center point and a radius.
circle :: Point -> Double -> Drawing
circle (cx, cy) r = Drawing
  [ tag False "circle"
    [ ("cx", showT cx)
    , ("cy", showT cy)
    , ("r", showT r)
    , ("fill", "none")
    , ("stroke", "black")
    , ("stroke-width", "0.1")
    ]
  ]


-- | Draws text at a point.
{-
text :: Point -> Text -> Drawing
text (x, y) msg = Drawing
  [ tag True "text"
    [ ("x", showT x)
    , ("y", showT y)
    , ("font-family", "Roboto")
    , ("fill", "black")
    , ("font-size", "6")
    ] <> msg <> "</text>"
  ]
  -}


-- | Draws a crosshair.
crosshairs :: Point -> Drawing
crosshairs (x, y) = mconcat
  [ line (x - 5, y) (x + 5, y)
  , line (x, y - 5) (x, y + 5)
  ]


-- | Draws a target: crosshair with a circle.
target :: Point -> Drawing
target (x, y) = circle (x, y) 2.5 <> crosshairs (x, y)


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
