{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module BindingTemplates
  ( allTemplates,
    checkRemount,
  )
where

import Control.Monad (forM_)
import Data.List (sortOn)
import Data.Text
  ( Text,
    pack,
    unpack,
  )
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)

-- | Hole type and position relative to reference line.
data Hole
  = -- | Pair of holes with specified width and position.
    Pair Double Double
  | -- | A single hole in the center with position.
    Center Double
  deriving (Eq)

-- | A template is a list of hole positions.
newtype Template = Template {holes :: [Hole]} deriving (Semigroup)

-- | Coordinates for a point in mm.
type Point = (Double, Double)

-- | Boot sole length in mm.
type Bsl = Double

-- | Shift forward or backward.
class Shift a where
  shift :: Double -> a -> a

instance Shift Hole where
  shift amount h = case h of
    Pair w y -> Pair w $ y + amount
    Center y -> Center $ y + amount

instance Shift Template where
  shift amount t = Template $ shift amount <$> holes t

-- | A drawing is collection of SVG elements.
newtype Drawing = Drawing [Text] deriving (Semigroup, Monoid)

-- | Place toe and heel pieces into one template based on Bsl.
placeToeHeel :: Template -> Template -> Bsl -> Template
placeToeHeel toe heel bsl = shift (bsl / 2) toe <> shift (-bsl / 2) heel

-- | Draw a template.
template :: Text -> Template -> Drawing
template msg t =
  centerLines
    <> scalingRulers
    <> mconcat (hole <$> holes t)
    <> text (pageCenter1 + 35, baseHeel + 5) (msg <> ", Heel")
    <> text (pageCenter2 + 35, baseHeel + 5) (msg <> ", Toe")

text :: Point -> Text -> Drawing
text (x, y) msg =
  Drawing
    [ tagOpen
        "text"
        [ ("x", showT x),
          ("y", showT y),
          ("class", "small"),
          ("transform", "rotate(90 " <> showT x <> " " <> showT y <> ")")
        ]
        <> msg
        <> tagClose "text"
    ]

-- | Draw a hole.
hole :: Hole -> Drawing
hole h = case h of
  Pair w y -> target' (w / 2) y <> target' (-w / 2) y
  Center y -> target' 0 y
  where
    target' x y
      | y >= 0 = target (pageCenter2 + x, baseToe - y)
      | otherwise = target (pageCenter1 + x, baseHeel - y)

-- | Draws X and Y centerling lines for the template,
--   which align with the mid sole mark and the mid line on the skis.
centerLines :: Drawing
centerLines =
  mconcat
    -- Center lines.
    [ line (pageCenter1, 0) (pageCenter1, pageHeight),
      line (pageCenter2, 0) (pageCenter2, pageHeight),
      -- Mount point lines.
      line (pageCenter2 - 40, baseToe) (pageCenter2 + 40, baseToe),
      line (pageCenter1 - 40, baseHeel) (pageCenter1 + 40, baseHeel),
      -- Center trim line.
      dashedLine (pageCenter, 0) (pageCenter, pageHeight),
      -- End trim lines.
      dashedLine (0, pageHeight) (pageCenter, pageHeight),
      dashedLine (pageCenter, 0) (pageWidth, 0),
      -- Notch trim lines.
      dashedLine (pageCenter1, baseHeel + 2) (pageCenter1 - 7, 0),
      dashedLine (pageCenter1, baseHeel + 2) (pageCenter1 + 7, 0),
      dashedLine (pageCenter2, baseToe - 2) (pageCenter2 - 6, pageHeight),
      dashedLine (pageCenter2, baseToe - 2) (pageCenter2 + 6, pageHeight)
    ]

-- | Draws some rulers to check for scaling.
scalingRulers :: Drawing
scalingRulers =
  mconcat $
    concat [rulerVertical, rulerHorizontal]
  where
    rulerLineVertical y = line (0, y) (width $ y - 5, y)
    rulerVertical = rulerLineVertical <$> [baseHeel .. baseToe]
    rulerLineHorizontal x = line (x, baseToe) (x, baseToe - width (x - pageCenter1))
    rulerHorizontal =
      rulerLineHorizontal
        <$> reverse [pageCenter1, pageCenter1 - 1 .. pageCenter1 - 40]
          <> [pageCenter1, pageCenter1 + 1 .. pageCenter1 + 40]
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

-- | Draws a line between two points.
line :: Point -> Point -> Drawing
line (x1, y1) (x2, y2) =
  Drawing
    [ tag
        "line"
        [ ("x1", showT x1),
          ("y1", showT y1),
          ("x2", showT x2),
          ("y2", showT y2),
          ("stroke", "black"),
          ("stroke-width", "0.1")
        ]
    ]

-- | Dashed line between two points.
dashedLine :: Point -> Point -> Drawing
dashedLine (x1, y1) (x2, y2) =
  Drawing
    [ tag
        "line"
        [ ("x1", showT x1),
          ("y1", showT y1),
          ("x2", showT x2),
          ("y2", showT y2),
          ("stroke", "black"),
          ("stroke-width", "0.1"),
          ("stroke-dasharray", "2 2")
        ]
    ]

-- | Draws a circle given a center point and a radius.
circle :: Point -> Double -> Drawing
circle (cx, cy) r =
  Drawing
    [ tag
        "circle"
        [ ("cx", showT cx),
          ("cy", showT cy),
          ("r", showT r),
          ("fill", "none"),
          ("stroke", "black"),
          ("stroke-width", "0.1")
        ]
    ]

-- | Draws a target.
target :: Point -> Drawing
target (x, y) =
  mconcat
    [ circle (x, y) 2.5,
      circle (x, y) 0.25,
      line (x - 0.25, y) (x - 0.5, y),
      line (x + 0.25, y) (x + 0.5, y),
      line (x, y - 0.25) (x, y - 0.5),
      line (x, y + 0.25) (x, y + 0.5),
      line (x - 3, y) (x - 2, y),
      line (x + 2, y) (x + 3, y),
      line (x, y - 3) (x, y - 2),
      line (x, y + 2) (x, y + 3)
    ]

-- | Converts Drawing to an SVG file.
svg :: Drawing -> Text
svg (Drawing elements) =
  T.unlines $
    [ "<?xml version=\"1.0\"?>",
      "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"",
      "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">",
      tagOpen
        "svg"
        [ ("xmlns", "http://www.w3.org/2000/svg"),
          ("version", "1.2"),
          ("width", showT pageWidth <> "mm"),
          ("height", showT pageHeight <> "mm"),
          ("viewBox", "0 0 " <> showT pageWidth <> " " <> showT pageHeight),
          ("style", "background-color:white")
        ],
      tagOpen "style" [],
      ".small { font: 6px sans-serif; }",
      tagClose "style"
    ]
      <> map ("  " <>) elements
      <> [tagClose "svg"]

-- | Formats an XML tag.
tag' :: Bool -> Text -> [(Text, Text)] -> Text
tag' open element attributes =
  "<"
    <> element
    <> mconcat
      [" " <> name <> "=\"" <> value <> "\"" | (name, value) <- attributes]
    <> (if open then " >" else " />")

tag :: Text -> [(Text, Text)] -> Text
tag = tag' False

tagOpen :: Text -> [(Text, Text)] -> Text
tagOpen = tag' True

tagClose :: Text -> Text
tagClose element = "</" <> element <> ">"

-- | Show for Text.
showT :: Show a => a -> Text
showT = pack . show

-- | Common Look toe.
lookToe :: Template
lookToe = Template [Pair 35 (-16.5), Pair 42 (-16.5 + 41.5)]

-- | Look Pivot.
pivot :: Bsl -> Template
pivot = placeToeHeel lookToe $ Template [Pair 21 82, Pair 29 (82 - 32)]

-- | Look SPX.
spx :: Bsl -> Template
spx = placeToeHeel lookToe $ Template [Pair 42 26, Pair 42 (26 - 105)]

-- | Look Rockerace.
rockerace :: Bsl -> Template
rockerace = placeToeHeel lookToe $ Template [Pair 42 59, Pair 42 (59 - 39)]

-- | Look R22 racing plate.
r22 :: Template
r22 = Template [Pair 12 164, Pair 35 99, Pair 35 (-52), Pair 35 (-171)]

-- | Salomon/Atomic Strive Demo.
striveDemo :: Template
striveDemo =
  Template
    [ pair toeBase,
      pair $ toeBase + toeLength,
      pair (-heelBase),
      pair (-heelBase - heelLength)
    ]
  where
    toeLength = 75
    heelLength = 80.5
    toeBase = 136
    heelBase = 144

    pair p = Pair 29.5 p

-- | Tyrolia PowerRail (PRD, Protector).
tyroliaPowerRail :: Template
tyroliaPowerRail = Template $ Pair 30 <$> [100, 200, -100, -200]

-- | Tyrolia SuperLiteRail XM (SLR, Protector).
tyroliaSuperLiteRailXm :: Template
tyroliaSuperLiteRailXm = Template $ Pair 25 <$> [90, 190, -110, -210]

-- | Tyrolia SuperLiteRail XL (SLR, Protector).
tyroliaSuperLiteRailXl :: Template
tyroliaSuperLiteRailXl = Template $ Pair 25 <$> [115, 200, -125, -210]

tyroliaAttackDemo :: Template
tyroliaAttackDemo =
  Template
    [ Pair 34 190,
      Pair 34 90,
      Pair 20 (-130),
      Pair 43 (-130 - 95)
    ]

-- | Rossignol nordic IFP.
rossignolIFP :: Int -> Template
rossignolIFP euroSize =
  Template
    [ Center 37,
      Pair 26 (-10),
      Center (-132),
      Center (-200 - offset * 13),
      Center (-235.5 - offset * 13)
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
shift' :: Bsl -> Template
shift' =
  placeToeHeel
    (Template [Center (-20 + 65), Pair 30 (-20 - 70)])
    (Template [Pair 36 15, Pair 36 (15 - 68)])

-- | Salomon STH2.
sth2Heel :: Template
sth2Heel = Template [Pair 32 28, Pair 32 (28 - 75)]

sth2 :: Bsl -> Template
sth2 = placeToeHeel (Template [Pair 42 (-15 + 30), Pair 40 (-15)]) sth2Heel

-- | Salomon Warden.
wardenToe :: Template
wardenToe = Template [Pair 40 (-15 + 65), Pair 40 (-15)]

warden11 :: Bsl -> Template
warden11 = placeToeHeel wardenToe (Template [Pair 30 28, Pair 30 (28 - 80)])

warden13 :: Bsl -> Template
warden13 = placeToeHeel wardenToe sth2Heel

-- | Marker Royal family (Jester, Griffon, Squire).
royal :: Bsl -> Template
royal =
  placeToeHeel
    royalToe
    (Template [Pair 32 25, Pair 32 (25 - 80)])

royalToe :: Template
royalToe = Template [Pair 36 (-12 + 31), Pair 36 (-12)]

xcompHeel :: Template
xcompHeel = Template [Pair 20 12, Pair 40.5 (12 - 80)]

-- | Marker XComp.
xcomp :: Bsl -> Template
xcomp = placeToeHeel (Template [Pair 36 (-12), Center (-12 - 55)]) xcompHeel

-- | Marker XCell.
xcell :: Bsl -> Template
xcell = placeToeHeel royalToe xcompHeel

-- | Marker Piston Plate.
pistonPlate :: Template
pistonPlate =
  Template
    [ Pair 36 $ 122 + 66,
      Pair 36 $ 122 + 46,
      Pair 36 $ 122,
      Pair 42 $ -122,
      Pair 42 $ -122 - 20,
      Pair 42 $ -122 - 51
    ]

-- | Head Tyrolia.
tyrolia :: Bsl -> Template
tyrolia =
  placeToeHeel
    (Template [Pair 40 (-15 + 55), Pair 40 (-15)])
    (Template [Pair 20 17, Pair 43.25 (17 - 95)])

-- | Head Tyrolia FreeFlex.
tyroliaFreeflex :: Bsl -> Template
tyroliaFreeflex bsl =
  shift (bsl / 2) $
    Template
      [ Pair 40 (innerToeHoles + 55),
        Pair 40 innerToeHoles,
        Pair 20 innerHeelHoles,
        Pair 43.25 $ innerHeelHoles - 95
      ]
  where
    innerToeHoles = -15
    innerHeelHoles = innerToeHoles + 31.5 - 2.5 - nearest
    nearest =
      snd $
        head $
          sortOn
            fst
            [(abs $ bsl - opt, opt) | opt <- [270, 280 .. 360]]

-- | Bishop BMF for NTN.
bmfNtn :: Bsl -> Template
bmfNtn bsl =
  shift (bsl / 2) $
    Template
      [ Pair 38 (-25),
        Pair 38 (-(25 + 38)),
        Pair 38 (-(25 + 38 + 38)),
        Pair 38 (-(25 + 38 + 38)),
        Center (-(25 + 38 + 38)),
        Center (-244),
        Center (-(244 + 38))
      ]

-- | Library of all alpine and telemark templates.
templateLibrary :: [(Text, Text, Bsl -> Template)]
templateLibrary =
  [ ("look-pivot", "Look Pivot", pivot),
    ("look-spx", "Looks SPX", spx),
    ("look-rockerace", "Look Rockerace", rockerace),
    ("salomon-shift", "Salomon Shift", shift'),
    ("salomon-sth2", "Salomon STH2", sth2),
    ("salomon-warden-11", "Salomon Warden 11, Strive 12/14", warden11),
    ("salomon-warden-13", "Salomon Warden 13, Strive 16", warden13),
    ("marker-royal", "Marker Royal (Jester, Griffon, etc)", royal),
    ("marker-xcomp", "Marker XComp", xcomp),
    ("marker-xcell", "Marker XCell", xcell),
    ("tyrolia", "Tyrolia", tyrolia),
    ("tyrolia-freeflex", "Tyrolia FreeFlex ST", tyroliaFreeflex),
    ("bishop-bmf-ntn", "Bishop", bmfNtn)
  ]

writeTemplate :: FilePath -> Text -> Template -> IO ()
writeTemplate file name t = writeFile file $ unpack $ svg $ template name t

-- | Generate binding templates.
allTemplates :: IO ()
allTemplates = do
  -- Alpine bindings.
  forM_ [240 .. 350 :: Int] $ \bsl ->
    forM_ templateLibrary $ \(name, desc, t) -> do
      createDirectoryIfMissing False $ unpack name
      writeTemplate
        (unpack name <> "/" <> unpack name <> "-bsl-" <> show bsl <> ".svg")
        (desc <> ", BSL: " <> showT bsl <> " mm")
        (t $ fromIntegral bsl)

  -- Alpine plate and demo bindings.
  writeTemplate "look-r22.svg" "Look R22 Plate" r22
  writeTemplate "marker-piston-plate.svg" "Marker Piston Plate" pistonPlate
  writeTemplate "salomon-strive-demo.svg" "Salomon Strive Demo" striveDemo
  writeTemplate "tyrolia-power-rail.svg" "Tyrolia PowerRail (PR)" tyroliaPowerRail
  writeTemplate "tyrolia-super-lite-rail-xm.svg" "Tyrolia SuperLiteRail XM (SLR)" tyroliaSuperLiteRailXm
  writeTemplate "tyrolia-super-lite-rail-xl.svg" "Tyrolia SuperLiteRail XL (SLR)" tyroliaSuperLiteRailXl
  writeTemplate "tyrolia-attack-demo.svg" "Tyrolia Attack Demo" tyroliaAttackDemo

  -- Nordic bindings.
  forM_ [36 .. 50] $ \euroSize -> do
    createDirectoryIfMissing False "rossignol-ifp"
    writeFile ("rossignol-ifp/rossignol-ifp-euro-" <> show euroSize <> ".svg") $
      unpack $
        svg $
          template ("Rossignol IFP, Euro Size: " <> showT euroSize) $
            rossignolIFP euroSize

-- | Returns the minimum distance between holes in a template, or multiple templates.
minimumHoleSpacing :: Template -> Double
minimumHoleSpacing (Template holes) = minimum [holeDistance a b | a <- holes, b <- holes, a /= b]

-- | Compute the distance between two sets of holes.  Ignores distance of a pair of holes.
holeDistance :: Hole -> Hole -> Double
holeDistance a b = case (a, b) of
  (Center a', Center b') -> abs $ a' - b'
  (Pair aW aP, Pair bW bP) -> sqrt $ (aW / 2 - bW / 2) ** 2 + (aP - bP) ** 2
  (Center aP, Pair bW bP) -> sqrt $ (bW / 2) ** 2 + (aP - bP) ** 2
  (a'@(Pair _ _), b'@(Center _)) -> holeDistance b' a'

-- | Checks if a binding remount will have enough clearance.
checkRemount :: IO ()
checkRemount = do
  putStrLn $ "Minimum hold distance: " <> show (minimumHoleSpacing $ tyroliaPowerRail <> tyroliaSuperLiteRailXl)
  writeTemplate "pr-to-slr.svg" "PR to SLR XL" $ tyroliaPowerRail <> tyroliaSuperLiteRailXl
