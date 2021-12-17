{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Main (main) where


import Control.Monad (forM_)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)


-- | Generate binding templates for pivot, spx, and shift bindings over a range of BSLs.
main :: IO ()
main = do
  generateTemplateLibrary
  customExample


-- | An example of a custom placement for a remount.
customExample :: IO ()
customExample = writeFile "custom-example.svg" $ unpack $ svg $ template
  [ PlaceToe  lookToe       334 0  -- Remounted with Look Rockerace 15 back 4 mm
  , PlaceHeel rockeraceHeel 334 0  -- to maximize hole separation.
  , PlaceHeel pivotHeel     334 0  -- to maximize hole separation.
  ]


-- | Generate all templates over a range of BSLs with a mount point of 0.
generateTemplateLibrary :: IO ()
generateTemplateLibrary = do

  -- Alpine bindings.
  forM_ [250 .. 340 :: Int] $ \ bsl ->
    forM_ templateLibrary $ \ (name, toe, heel) -> do
      createDirectoryIfMissing False $ unpack name
      writeFile (unpack name <> "/" <> unpack name <> "-bsl-" <> show bsl <> ".svg") $
        unpack $ svg $ template
          [ PlaceToe toe (fromIntegral bsl) 0
          , PlaceHeel heel (fromIntegral bsl) 0
          ]

  -- Alpine plate bindings.
  writeFile "look-r22.svg" $ unpack $ svg $ template [PlacePlate r22 0]

  -- Nordic bindings.
  forM_ [36 .. 50] $ \ euroSize -> do
      createDirectoryIfMissing False "rossignol-ifp"
      writeFile ("rossignol-ifp/rossignol-ifp-euro-" <> show euroSize <> ".svg") $
        unpack $ svg $ template [PlacePlate (rossignolIFP euroSize) 0]


-- | Library of all alpine and telemark templates.
templateLibrary :: [(Text, ToeBinding, HeelBinding)]
templateLibrary = 
  [ ("look-pivot", lookToe, pivotHeel)
  , ("look-spx", lookToe, spxHeel)
  , ("look-r20", lookR20Toe, lookR20Heel)
  , ("look-rockerace", lookToe, rockeraceHeel)
  , ("marker-royal", royalToe, royalHeel)
  , ("salomon-shift", shiftToe, shiftHeel)
  , ("salomon-sth2", sth2Toe, sth2Heel)
  , ("salomon-warden", wardenToe, sth2Heel)
  , ("tyrolia", tyroliaToe, tyroliaHeel)
  , ("bishop-bmf-ntn", bmfNtnToe, noHeel)
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
data Hole
  = Pair Double Double  -- ^ Pair of holes: width and distance from reference line.
  | Center Double       -- ^ A single hole in the center, given distance from reference line.


-- | Coordinates for a point in mm.
type Point = (Double, Double)


-- | Boot sole length in mm.
type BSL = Double


-- | Mount point, + in front of the line, - behind the line, in mm.
type MountPoint = Double


-- | Look Pivot heel.
pivotHeel :: HeelBinding
pivotHeel = HeelBinding
  [ Pair 21 82
  , Pair 29 (82 - 32)
  ]


-- | Look SPX heel.
spxHeel :: HeelBinding
spxHeel = HeelBinding
  [ Pair 42 26
  , Pair 42 (26 - 105)
  ]


-- | Look Rockerace heel.
rockeraceHeel :: HeelBinding
rockeraceHeel = HeelBinding
  [ Pair 42 59 
  , Pair 42 (59 - 39)
  ]


-- | Look toe.
lookToe :: ToeBinding
lookToe = ToeBinding
  [ Pair 35 (- 16.5)
  , Pair 42 (- 16.5 + 41.5)
  ]


-- | Look R20 racing plate toe.
lookR20Toe :: ToeBinding
lookR20Toe = ToeBinding
  [ Pair 37 (- (16.5 + 33))
  , Pair 35 (110 - (16.5 + 33))
  ]


-- | Look R20 racing plate heel.
lookR20Heel :: HeelBinding
lookR20Heel = HeelBinding
  [ Pair 34 (47.5 + 26)
  , Pair 36 (47.5 + 26 - 119)
  ]


-- | Look R22 racing plate.
r22 :: Plate
r22 = Plate
  [ Pair 12 163
  , Pair 35 98
  , Pair 35 (- 52)
  , Pair 35 (- (52 + 120))
  ]


-- | Rossignol nordic IFP.
rossignolIFP :: Int -> Plate
rossignolIFP euroSize = Plate
  [ Center 37
  , Pair 26 (- 10)
  , Center (- 132)
  , Center (- 200   - offset * 13)
  , Center (- 235.5 - offset * 13)
  ]

  where

  offset :: Double
  offset
    | euroSize >= 36 && euroSize <= 38 = 0
    | euroSize >= 39 && euroSize <= 41 = 1
    | euroSize >= 42 && euroSize <= 44 = 2
    | euroSize >= 45 && euroSize <= 47 = 3
    | euroSize >= 48 && euroSize <= 50 = 4
    | otherwise = error $ "Unsupported euro size: " <> show euroSize
    

-- | Salomon Shift.
shiftToe :: ToeBinding
shiftToe = ToeBinding
  [ Center (- 20 + 65)
  , Pair 30 (- 20 - 70)
  ]

shiftHeel :: HeelBinding
shiftHeel = HeelBinding
  [ Pair 36 15
  , Pair 36 (15 - 68)
  ]


-- | Salomon STH2.
sth2Toe :: ToeBinding
sth2Toe = ToeBinding
  [ Pair 42 (- 15 + 30)
  , Pair 40 (- 15)
  ]

sth2Heel :: HeelBinding
sth2Heel = HeelBinding
  [ Pair 32 28
  , Pair 32 (28 - 75)
  ]


-- | Salomon Warden toe.
wardenToe :: ToeBinding
wardenToe = ToeBinding
  [ Pair 40 (- 15 + 65)
  , Pair 40 (- 15)
  ]


-- | Marker Royal family (Jester, Griffon, Squire).
royalToe :: ToeBinding
royalToe = ToeBinding
    [ Pair 36 (- 12 + 31)
    , Pair 36 (- 12)
    ]

royalHeel :: HeelBinding
royalHeel = HeelBinding
    [ Pair 32 25
    , Pair 32 (25 - 80)
    ]


-- | Tyrolia.
tyroliaToe :: ToeBinding
tyroliaToe = ToeBinding
    [ Pair 40 (- 15 + 55)
    , Pair 40 (- 15)
    ]

tyroliaHeel :: HeelBinding
tyroliaHeel = HeelBinding
    [ Pair 20 17
    , Pair 42.5 (17 - 95)
    ]


-- | Bishop BMF for NTN.
bmfNtnToe :: ToeBinding
bmfNtnToe = ToeBinding
  [ Pair 38 (- 25)
  , Pair 38 (- (25 + 38))
  , Pair 38 (- (25 + 38 + 38))
  , Pair 38 (- (25 + 38 + 38))
  , Center (- (25 + 38 + 38))
  , Center (- 244)
  , Center (- (244 + 38))
  ]

noHeel :: HeelBinding
noHeel = HeelBinding []


-- | Generate a template for a set of binding placements.
template :: [Placement] -> Drawing
template placements = centerLines <> scalingRulers <> mconcat (placement <$> placements)


-- | Draw the holes for a binding placement.
placement :: Placement -> Drawing
placement = \case

  PlaceToe (ToeBinding holes) bsl mount ->
    mconcat ((hole $ mount + bsl / 2) <$> holes)

  PlaceHeel (HeelBinding holes) bsl mount ->
    mconcat ((hole $ mount - bsl / 2) <$> holes)

  PlacePlate (Plate holes) mount -> mconcat $ hole mount <$> holes


-- | Draw a hole with a y bias.
hole :: Double -> Hole -> Drawing
hole bias = \case
  Pair w y -> target' (w / 2) y <> target' (- w / 2) y
  Center y -> target' 0 y

  where

  target' x y
    | y' >= 0 = target (pageCenter2 + x, baseToe - y')
    | otherwise = target (pageCenter1 + x, baseHeel - y') 

    where

    y' = y + bias


-- | Draws X and Y centerling lines for the template,
--   which align with the mid sole mark and the mid line on the skis.
centerLines :: Drawing
centerLines = mconcat $
  -- Center lines.
  [ dashedLine (pageCenter, 0) (pageCenter, pageHeight)
  , line (pageCenter1, 0) (pageCenter1, pageHeight)
  , line (pageCenter2, 0) (pageCenter2, pageHeight)
  ] <>

  -- Center mount lines.
  [ line (pageCenter2 - 40, baseToe) (pageCenter2 + 40, baseToe)
  , line (pageCenter1 - 40, baseHeel) (pageCenter1 + 40, baseHeel)
  ]


-- | Draws some rulers to check for scaling.
scalingRulers :: Drawing
scalingRulers = mconcat $ concat
  [ line' <$> [baseHeel .. baseToe]
  , line'' <$> m
  ]

  where

  m = reverse [pageCenter1, pageCenter1 - 1 .. pageCenter1 - 40] <>
    [pageCenter1, pageCenter1 + 1 .. pageCenter1 + 40]

  line' y = line (0, y) (width $ y - 5, y)
  line'' x = line (x, baseToe) (x, baseToe - width (x - pageCenter1))

  width :: Double -> Double
  width n
    | (round n :: Int) `mod` 10 == 0 = 3
    | (round n :: Int) `mod` 5 == 0 = 2
    | otherwise = 1


-- | Page dimensions and parameters.

pageWidth :: Double
pageWidth = 190

pageHeight :: Double
pageHeight = 259

pageCenter :: Double
pageCenter = pageWidth / 2

pageCenter1 :: Double
pageCenter1 = pageCenter / 2

pageCenter2 :: Double
pageCenter2 = pageCenter / 2 + pageCenter

baseToe :: Double
baseToe = 255

baseHeel :: Double
baseHeel = 5


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
    , ("stroke-dasharray", "2 2")
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


-- | Draws a target.
target :: Point -> Drawing
target (x, y) = mconcat
  [ circle (x, y) 2.5
  , line (x - 0.5, y) (x + 0.5, y)
  , line (x, y - 0.5) (x, y + 0.5)
  , line (x - 3, y) (x - 2, y)
  , line (x + 2, y) (x + 3, y)
  , line (x, y - 3) (x, y - 2)
  , line (x, y + 2) (x, y + 3)
  ]


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
