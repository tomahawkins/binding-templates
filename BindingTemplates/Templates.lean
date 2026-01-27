inductive Holes where
  | center : Float → Holes        -- Y position.
  | pair : Float → Float → Holes  -- Width, Y position.
  | triple : Float → Float → Holes

def center := Holes.center
def pair := Holes.pair
def triple := Holes.triple

def shiftHoles (offset : Float) (h : Holes) : Holes :=
  match h with
  | Holes.center y => center (y + offset)
  | Holes.pair w y => pair w (y + offset)
  | Holes.triple w y => triple w (y + offset)

def holeInFront : Holes → Bool
  | Holes.center y => y >= 0
  | Holes.pair _w y => y >= 0
  | Holes.triple _w y => y >= 0

-- Split holes from front (positive) and back (negative) of mount line.
def partitionHoles (h : List Holes) : List Holes × List Holes :=
  List.partition holeInFront h

-- A template is either for a plate or system binding or for a regular separate toe-heal binding pair.
inductive TemplateType where
  | plate : List Holes → TemplateType  -- Holes are specified relative to mount line.
  | regular : List Holes → List Holes → TemplateType  -- Toe holes and heel holes are specified relative to edge of boot.
  | custom : (Float → List Holes) → TemplateType

def plate := TemplateType.plate
def regular := TemplateType.regular
def custom := TemplateType.custom

structure Template where
  company : String
  file : String
  description : String
  template : TemplateType


-- Templates.

namespace Atomic

  def icon : Template :=
    { company := "Atomic",
      file := "Icon",
      description := "Icon Plate",
      template := plate
        [ pair 35 100,
          pair 35 (100 + 76),
          pair 42 (-118),
          pair 42 (-118 - 60)
        ]
    }

  def templates : List Template := [icon]

end Atomic

namespace Look

  def toe : List Holes :=
    [pair 35 (-16.5), pair 42 (-16.5 + 41.5)]

  def pivot : Template :=
    { company := "Look",
      file := "Pivot",
      description := "Pivot",
      template := regular toe [pair 21 82, pair 29 (82 - 32)]
    }

  def spx : Template :=
    { company := "Look",
      file := "SPX",
      description := "SPX",
      template := regular toe [pair 42 26, pair 42 (26 - 105)]
    }

  def rockerace : Template :=
    { company := "Look",
      file := "Rockerace",
      description := "Rockerace",
      template := regular toe [pair 42 59, pair 42 (59 - 39)]
    }

  def r22 : Template :=
    { company := "Look",
      file := "R22",
      description := "R22 Plate",
      template := plate
        [ pair 12 164,
          pair 35 99,
          pair 35 (-52),
          pair 35 (-171)
        ]
    }

  def templates : List Template := [pivot, spx, rockerace, r22]

end Look


namespace Salomon

  def striveDemo : Template :=
    { company := "Salomon",
      file := "StriveDemo",
      description := "Strive Demo",
      template :=
        let toeLength := 75
        let heelLength := 80.5
        let toeBase := 136
        let heelBase := 144
        let pair' p := pair 29.5 p
        plate
          [ pair' toeBase,
            pair' (toeBase + toeLength),
            pair' (-heelBase),
            pair' (-heelBase - heelLength)
          ]
    }

  def shift : Template :=
    { company := "Salomon",
      file := "Shift",
      description := "Shift",
      template := regular
        [center (-20 + 65), pair 30 (-20 - 70)]
        [pair 36 15, pair 36 (15 - 68)]
    }

  def sth2Heel : List Holes :=
    [pair 32 28, pair 32 (28 - 75)]

  def sth2 : Template :=
    { company := "Salomon",
      file := "STH2",
      description := "STH2",
      template := regular [pair 42 (-15 + 30), pair 40 (-15)] sth2Heel
    }

  def wardenToe : List Holes :=
    [pair 40 (-15 + 65), pair 40 (-15)]

  def warden11 : Template :=
    { company := "Salomon",
      file := "Warden-11",
      description := "Warden 11, Strive 12/14",
      template := regular wardenToe [pair 30 28, pair 30 (28 - 80)]
    }

  def warden13 : Template :=
    { company := "Salomon",
      file := "Warden-13",
      description := "Warden 13, Strive 16",
      template := regular wardenToe sth2Heel
    }

  def templates : List Template := [striveDemo, shift, sth2, warden11, warden13]

end Salomon


namespace Tyrolia

  def powerRail : Template :=
    { company := "Tyrolia",
      file := "PowerRail",
      description := "PowerRail (PR)",
      template := plate
        [ pair 30 200,
          pair 30 100,
          pair 30 (-100),
          pair 30 (-200)
        ]
      }

  def superLiteRail (front : Float) (back : Float) : TemplateType :=
    plate [pair 25 front, pair 25 (front + 85), pair 25 (-back), pair 25 (-back - 85)]

  def superLiteRailXs : Template :=
    { company := "Tyrolia",
      file := "SuperLiteRailXS",
      description := "SuperLiteRail XS (SLR)",
      template := superLiteRail 82 102
    }

  def superLiteRailXm : Template :=
    { company := "Tyrolia",
      file := "SuperLiteRailXM",
      description := "SuperLiteRail XM (SLR)",
      template := superLiteRail 90 110
    }

  def superLiteRailXl : Template :=
    { company := "Tyrolia",
      file := "SuperLiteRailXL",
      description := "SuperLiteRail XL (SLR)",
      template := superLiteRail 110 130
    }

  def attackDemo : Template :=
    { company := "Tyrolia",
      file := "AttackDemo",
      description := "Attack Demo",
      template := plate
        [ pair 34 190,
          pair 34 90,
          pair 20 (-130),
          pair 43 (-130 - 95)
        ]
    }

  def tyrolia : Template :=
    { company := "Tyrolia",
      file := "Common",
      description := "Tyrolia Common",
      template := regular [pair 40 (-15 + 55), pair 40 (-15)] [pair 20 17, pair 43.25 (17 - 95)]
    }

  def freeflexFind (bsl : Float) (diffSofar : Float) (optSofar : Float) (rest : List Float) : Float :=
    match rest with
    | [] => optSofar
    | opt :: opts =>
      let diff := Float.abs (bsl - opt)
      let opt' := if diff < diffSofar then opt else optSofar
      let diff' := if diff < diffSofar then diff else diffSofar
      freeflexFind bsl diff' opt' opts

  def freeflex : Template :=
    { company := "Tyrolia",
      file := "FreeFlexST",
      description := "FreeFlex ST",
      template := custom (λ bsl =>
        let innerToeHoles := -15
        let opts := [270, 280, 290, 300, 310, 320, 330, 340, 350, 360]
        let nearest := freeflexFind bsl 100 100 opts
        let innerHeelHoles := innerToeHoles + 31.5 - 2.5 - nearest
        List.map (shiftHoles (bsl / 2))
          [ pair 40 (innerToeHoles + 55),
            pair 40 innerToeHoles,
            pair 20 innerHeelHoles,
            pair 43.25 (innerHeelHoles - 95)
          ])
    }

    def templates : List Template :=
      [powerRail, superLiteRailXs, superLiteRailXm, superLiteRailXl, attackDemo, tyrolia, freeflex]

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

  def royalToe : List Holes :=
    [pair 36 (-12 + 31), pair 36 (-12)]

  def royal : Template :=
    { company := "Marker",
      file := "Royal",
      description := "Royal (Jester, Griffon, etc)",
      template := regular royalToe [pair 32 25, pair 32 (25 - 80)]
    }

  def xcompHeel : List Holes :=
    [pair 20 12, pair 40.5 (12 - 80)]

  def xcomp : Template :=
    { company := "Marker",
      file := "XComp",
      description := "XComp",
      template := regular
        [pair 36 (-12), center (-12 - 55)]
        xcompHeel
    }

  def xcell : Template :=
    { company := "Marker",
      file := "XCell",
      description := "XCell",
      template := regular royalToe xcompHeel
    }

  def pistonPlate : Template :=
    { company := "Marker",
      file := "PistonPlate",
      description := "Piston Plate",
      template := plate
        [ pair 36 (122 + 66),
          pair 36 (122 + 46) ,
          pair 36 (122),
          pair 42 (-122),
          pair 42 (-122 - 20),
          pair 42 (-122 - 51)
        ]
    }

  def fdt : Template :=
    { company := "Marker",
      file := "FdtPlate",
      description := "FDT Plate",
      template :=
        let pair' p := pair 36 p
        plate
          [ pair' (75 + 110),
            pair' 110,
            pair' (-140),
            pair' (-140 - 80)
          ]
    }

  def templates : List Template := [royal, xcomp, xcell, pistonPlate, fdt]

end Marker

namespace Bishop

  def bmfNtn : Template :=
    { company := "Bishop",
      file := "BMF-NTN",
      description := "BMF NTN",
      template := custom (λ bsl => List.map (shiftHoles (bsl / 2))
        [ pair 38 (-25),
          pair 38 (-(25 + 38)),
          triple 38 (-(25 + 38 + 38)),
          center (-244),
          center (-(244 + 38))
        ])
    }

  def templates : List Template := [bmfNtn]

end Bishop

def templates : List Template :=
  Atomic.templates ++
  Look.templates ++
  Salomon.templates ++
  Tyrolia.templates ++
  Marker.templates ++
  Bishop.templates
