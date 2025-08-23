-- A template is a tree of holes.
inductive Template where
  | hole : Float -> Float -> Template
  | holes : Template -> Template -> Template

instance : Append Template where
  append := Template.holes

-- Create a hole on the center line.
def center (y : Float) : Template :=
  Template.hole 0 y

-- Create a pair of holes.
def pair (width y : Float) : Template :=
  Template.holes (Template.hole (-width / 2) y) (Template.hole (width / 2) y)

-- Shift a template vertically by a given distance.
def shift (dy : Float) : Template → Template
  | Template.hole x y =>  Template.hole x (y + dy)
  | Template.holes t1 t2 => Template.holes (shift dy t1) (shift dy t2)

-- Place toe and heel pieces into one template based on Bsl.
def placeToeHeel (toe heel : Template) (bsl : Float) : Template :=
  shift (bsl / 2) toe ++ shift (-bsl / 2) heel

-- A template parameterized for BSL.
def BslTemplate := Float → Template

-- Concats a list of Templates into one.
def concatTemplates : (t : List Template) → Template
  | [] => center 0 -- Error case, should not happen.
  | a :: [] => a
  | a :: rest => a ++ concatTemplates rest



-- Templates.

namespace Look

  def toe : Template :=
     pair 35 (-16.5) ++ pair 42 (-16.5 + 41.5)

  def pivot : BslTemplate :=
    placeToeHeel toe (pair 21 82 ++ pair 29 (82 - 32))

  def spx : BslTemplate :=
    placeToeHeel toe (pair 42 26 ++ pair 42 (26 - 105))

  def rockerace : BslTemplate :=
    placeToeHeel toe (pair 42 59 ++ pair 42 (59 - 39))

  def r22 : Template :=
    pair 12 164 ++
    pair 35 99 ++
    pair 35 (-52) ++
    pair 35 (-171)

end Look


namespace Salomon

  def striveDemo : Template :=
    let toeLength := 75
    let heelLength := 80.5
    let toeBase := 136
    let heelBase := 144
    let pair' p := pair 29.5 p
    pair' toeBase ++
    pair' (toeBase + toeLength) ++
    pair' (-heelBase) ++
    pair' (-heelBase - heelLength)

  def shift : BslTemplate :=
    placeToeHeel
      (center (-20 + 65) ++ pair 30 (-20 - 70))
      (pair 36 15 ++ pair 36 (15 - 68))

  def sth2Heel : Template :=
    pair 32 28 ++ pair 32 (28 - 75)

  def sth2 : BslTemplate :=
    placeToeHeel (pair 42 (-15 + 30) ++ pair 40 (-15)) sth2Heel

  def wardenToe : Template :=
    pair 40 (-15 + 65) ++ pair 40 (-15)

  def warden11 : BslTemplate :=
    placeToeHeel wardenToe (pair 30 28 ++ pair 30 (28 - 80))

  def warden13 : BslTemplate :=
    placeToeHeel wardenToe sth2Heel

end Salomon


namespace Tyrolia

  def powerRail : Template :=
    concatTemplates (pair 30 <$> [100, 200, -100, -200])

  def superLiteRailXm : Template :=
    concatTemplates (pair 25 <$> [90, 190, -110, -210])

  def superLiteRailXl : Template :=
    concatTemplates (pair 25 <$> [115, 200, -125, -210])

  def attackDemo : Template :=
    pair 34 190 ++
    pair 34 90 ++
    pair 20 (-130) ++
    pair 43 (-130 - 95)

  def tyrolia : BslTemplate :=
    placeToeHeel
      (pair 40 (-15 + 55) ++ pair 40 (-15))
      (pair 20 17 ++ pair 43.25 (17 - 95))

  def freeflexFind (bsl : Float) (diffSofar : Float) (optSofar : Float) (rest : List Float) : Float :=
    match rest with
    | [] => optSofar
    | opt :: opts =>
      let diff := Float.abs (bsl - opt)
      let opt' := if diff < diffSofar then opt else optSofar
      let diff' := if diff < diffSofar then diff else diffSofar
      freeflexFind bsl diff' opt' opts

  def freeflex (bsl : Float) : Template :=
    let innerToeHoles := -15
    let opts := [270, 280, 290, 300, 310, 320, 330, 340, 350, 360]
    let nearest := freeflexFind bsl 100 100 opts
    let innerHeelHoles := innerToeHoles + 31.5 - 2.5 - nearest
    shift (bsl / 2) (
      pair 40 (innerToeHoles + 55) ++
      pair 40 innerToeHoles ++
      pair 20 innerHeelHoles ++
      pair 43.25 (innerHeelHoles - 95)
    )

end Tyrolia


/-
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
-/

namespace Marker

  def royalToe : Template :=
    pair 36 (-12 + 31) ++ pair 36 (-12)

  def royal : BslTemplate :=
    placeToeHeel
      royalToe
      (pair 32 25 ++ pair 32 (25 - 80))

  def xcompHeel : Template :=
    pair 20 12 ++ pair 40.5 (12 - 80)

  def xcomp : BslTemplate :=
    placeToeHeel
      (pair 36 (-12) ++ center (-12 - 55))
       xcompHeel

  def xcell : BslTemplate :=
    placeToeHeel royalToe xcompHeel

  def pistonPlate : Template :=
      pair 36 (122 + 66) ++
      pair 36 (122 + 46) ++
      pair 36 (122) ++
      pair 42 (-122) ++
      pair 42 (-122 - 20) ++
      pair 42 (-122 - 51)

  def system : Template :=
    let pair' p := pair 36 p
    pair' (75 + 110) ++
    pair' 110 ++
    pair' (-140) ++
    pair' (-140 - 80)

end Marker

namespace Bishop

  def bmfNtn (bsl : Float) : Template :=
    shift (bsl / 2) (
      pair 38 (-25) ++
      pair 38 (-(25 + 38)) ++
      pair 38 (-(25 + 38 + 38)) ++
      pair 38 (-(25 + 38 + 38)) ++
      center (-(25 + 38 + 38)) ++
      center (-244) ++
      center (-(244 + 38))
    )

end Bishop

/-
  -- Nordic bindings.
  forM_ [36 .. 50] $ \euroSize -> do
    createDirectoryIfMissing False "rossignol-ifp"
    writeFile ("rossignol-ifp/rossignol-ifp-euro-" <> show euroSize <> ".svg") $
      unpack $
        svg $
          template ("Rossignol IFP, Euro Size: " <> showT euroSize) $
            rossignolIFP euroSize

-- | Returns the minimum distance between holes in a template, or multiple templates.
def minimumHoleSpacing : Template -> Double :=
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
  putStrLn $ "Minimum hole distance: " <> show (minimumHoleSpacing $ tyroliaPowerRail <> pistonPlate)
-/
