
namespace Types

structure Template where
  holes : List Hole
deriving BEq

inductive Hole where
  | Pair : Float → Float → Hole
  | Center : Float → Hole
deriving BEq

end Types


/-


--import Mathlib.Init.Data.Float.Basic
--import Mathlib.Data.String.Basic
--import Mathlib.Data.List.Basic
--import Mathlib.Data.Nat.Prime
--import Mathlib.Tactic.LibrarySearch
import Std

namespace BindingTemplates

inductive Hole where
  | Pair : Float → Float → Hole
  | Center : Float → Hole
deriving BEq

structure Template where
  holes : List Hole
deriving BEq

instance : Append Template where
  append t1 t2 := { holes := t1.holes ++ t2.holes }

structure Drawing where
  elements : List String := []
deriving BEq

instance : Append Drawing where
  append d1 d2 := { elements := d1.elements ++ d2.elements }

instance : EmptyCollection Drawing where
  emptyCollection := { elements := [] }

abbrev Point : Type := Float × Float

abbrev Bsl : Type := Float

class Shift (α : Type) where
  shift : Float → α → α

instance : Shift Hole where
  shift amount h := match h with
    | Hole.Pair w y => Hole.Pair w (y + amount)
    | Hole.Center y => Hole.Center (y + amount)

instance : Shift Template where
  shift amount t := { holes := t.holes.map (Shift.shift amount) }

def placeToeHeel (toe : Template) (heel : Template) (bsl : Bsl) : Template :=
  (Shift.shift (bsl / 2) toe) ++ (Shift.shift (-bsl / 2) heel)

def template (msg : String) (t : Template) : Drawing :=
  centerLines ++ scalingRulers ++ (t.holes.map hole).foldl Append.append {} ++
  text (pageCenter1 + 35, baseHeel + 5) (msg ++ ", Heel") ++
  text (pageCenter2 + 35, baseHeel + 5) (msg ++ ", Toe")

def text (p : Point) (msg : String) : Drawing :=
  let (x, y) := p
  { elements := [s!"<text x=\"{x}\" y=\"{y}\" class=\"small\" transform=\"rotate(90 {x} {y})\">{msg}</text>"] }

def hole (h : Hole) : Drawing :=
  match h with
  | Hole.Pair w y => target' (w / 2) y ++ target' (-w / 2) y
  | Hole.Center y => target' 0 y
where
  target' (x y : Float) : Drawing :=
    if y >= 0 then
      target (pageCenter2 + x, baseToe - y)
    else
      target (pageCenter1 + x, baseHeel - y)

def centerLines : Drawing :=
  -- Center lines.
  line (pageCenter1, 0) (pageCenter1, pageHeight) ++
  line (pageCenter2, 0) (pageCenter2, pageHeight) ++
  -- Mount point lines.
  line (pageCenter2 - 40, baseToe) (pageCenter2 + 40, baseToe) ++
  line (pageCenter1 - 40, baseHeel) (pageCenter1 + 40, baseHeel) ++
  -- Center trim line.
  dashedLine (pageCenter, 0) (pageCenter, pageHeight) ++
  -- End trim lines.
  dashedLine (0, pageHeight) (pageCenter, pageHeight) ++
  dashedLine (pageCenter, 0) (pageWidth, 0) ++
  -- Notch trim lines.
  dashedLine (pageCenter1, baseHeel + 2) (pageCenter1 - 7, 0) ++
  dashedLine (pageCenter1, baseHeel + 2) (pageCenter1 + 7, 0) ++
  dashedLine (pageCenter2, baseToe - 2) (pageCenter2 - 6, pageHeight) ++
  dashedLine (pageCenter2, baseToe - 2) (pageCenter2 + 6, pageHeight)

partial def scalingRulers : Drawing :=
  (rulerVertical ++ rulerHorizontal).foldl Append.append {}
where
  rulerLineVertical (y : Float) : Drawing :=
    line (0, y) (width (y - 5), y)  -- Note: width takes (y - 5)? No, in code it's width $ y -5 ? Wait, no: line (0,y) (width $ y-5, y)
    -- Wait, looking back: line (0, y) (width $ y - 5, y)
    -- Yes, width (y - 5)
  rulerVertical : List Drawing :=
    (gen baseHeel baseToe).map rulerLineVertical
  rulerLineHorizontal (x : Float) : Drawing :=
    line (x, baseToe) (x, baseToe - width (x - pageCenter1))
  rulerHorizontal : List Drawing :=
    (genLeft ++ genRight).map rulerLineHorizontal
  gen (start end : Float) : List Float :=
    if start > end then [] else start :: gen (start + 1) end
  genDecreasing (start end : Float) : List Float :=
    if start < end then [] else start :: genDecreasing (start - 1) end
  genLeft : List Float :=
    (genDecreasing pageCenter1 (pageCenter1 - 40)).reverse
  genRight : List Float :=
    gen pageCenter1 (pageCenter1 + 40)
  width (n : Float) : Float :=
    let rn := n.round.toInt
    if rn % 10 == 0 then 3
    else if rn % 5 == 0 then 2
    else 1

def pageWidth : Float := 190
def pageHeight : Float := 259
def pageCenter : Float := pageWidth / 2
def pageCenter1 : Float := pageCenter / 2
def pageCenter2 : Float := pageCenter / 2 + pageCenter
def baseToe : Float := 255
def baseHeel : Float := 5

def line (p1 p2 : Point) : Drawing :=
  let (x1, y1) := p1
  let (x2, y2) := p2
  { elements := [s!"<line x1=\"{x1}\" y1=\"{y1}\" x2=\"{x2}\" y2=\"{y2}\" stroke=\"black\" stroke-width=\"0.1\" />"] }

def dashedLine (p1 p2 : Point) : Drawing :=
  let (x1, y1) := p1
  let (x2, y2) := p2
  { elements := [s!"<line x1=\"{x1}\" y1=\"{y1}\" x2=\"{x2}\" y2=\"{y2}\" stroke=\"black\" stroke-width=\"0.1\" stroke-dasharray=\"2 2\" />"] }

def circle (p : Point) (r : Float) : Drawing :=
  let (cx, cy) := p
  { elements := [s!"<circle cx=\"{cx}\" cy=\"{cy}\" r=\"{r}\" fill=\"none\" stroke=\"black\" stroke-width=\"0.1\" />"] }

def target (p : Point) : Drawing :=
  let (x, y) := p
  circle (x, y) 2.5 ++
  circle (x, y) 0.25 ++
  line (x - 0.25, y) (x - 0.5, y) ++
  line (x + 0.25, y) (x + 0.5, y) ++
  line (x, y - 0.25) (x, y - 0.5) ++
  line (x, y + 0.25) (x, y + 0.5) ++
  line (x - 3, y) (x - 2, y) ++
  line (x + 2, y) (x + 3, y) ++
  line (x, y - 3) (x, y - 2) ++
  line (x, y + 2) (x, y + 3)

def lookToe : Template :=
  { holes := [Hole.Pair 35 (-16.5), Hole.Pair 42 (-16.5 + 41.5)] }

def pivot (bsl : Bsl) : Template :=
  placeToeHeel lookToe { holes := [Hole.Pair 21 82, Hole.Pair 29 (82 - 32)] } bsl

def spx (bsl : Bsl) : Template :=
  placeToeHeel lookToe { holes := [Hole.Pair 42 26, Hole.Pair 42 (26 - 105)] } bsl

def rockerace (bsl : Bsl) : Template :=
  placeToeHeel lookToe { holes := [Hole.Pair 42 59, Hole.Pair 42 (59 - 39)] } bsl

def r22 : Template :=
  { holes := [Hole.Pair 12 164, Hole.Pair 35 99, Hole.Pair 35 (-52), Hole.Pair 35 (-171)] }

def striveDemo : Template :=
  let toeLength := 75
  let heelLength := 80.5
  let toeBase := 136
  let heelBase := 144
  let pair (p : Float) := Hole.Pair 29.5 p
  { holes := [pair toeBase, pair (toeBase + toeLength), pair (-heelBase), pair (-heelBase - heelLength)] }

def tyroliaPowerRail : Template :=
  { holes := (List.map (Hole.Pair 30) [100, 200, -100, -200]) }

def tyroliaSuperLiteRailXm : Template :=
  { holes := (List.map (Hole.Pair 25) [90, 190, -110, -210]) }

def tyroliaSuperLiteRailXl : Template :=
  { holes := (List.map (Hole.Pair 25) [115, 200, -125, -210]) }

def tyroliaAttackDemo : Template :=
  { holes := [Hole.Pair 34 190, Hole.Pair 34 90, Hole.Pair 20 (-130), Hole.Pair 43 (-130 - 95)] }

def rossignolIFP (euroSize : Int) : Template :=
  let offset : Float :=
    if euroSize >= 36 && euroSize <= 38 then 0
    else if euroSize >= 39 && euroSize <= 41 then 1
    else if euroSize >= 42 && euroSize <= 44 then 2
    else if euroSize >= 45 && euroSize <= 47 then 3
    else if euroSize >= 48 && euroSize <= 50 then 4
    else panic! s!"Unsupported euro size: {euroSize}"
  { holes := [Hole.Center 37, Hole.Pair 26 (-10), Hole.Center (-132), Hole.Center (-200 - offset * 13), Hole.Center (-235.5 - offset * 13)] }

def shift' (bsl : Bsl) : Template :=
  placeToeHeel { holes := [Hole.Center (-20 + 65), Hole.Pair 30 (-20 - 70)] } { holes := [Hole.Pair 36 15, Hole.Pair 36 (15 - 68)] } bsl

def sth2Heel : Template :=
  { holes := [Hole.Pair 32 28, Hole.Pair 32 (28 - 75)] }

def sth2 (bsl : Bsl) : Template :=
  placeToeHeel { holes := [Hole.Pair 42 (-15 + 30), Hole.Pair 40 (-15)] } sth2Heel bsl

def wardenToe : Template :=
  { holes := [Hole.Pair 40 (-15 + 65), Hole.Pair 40 (-15)] }

def warden11 (bsl : Bsl) : Template :=
  placeToeHeel wardenToe { holes := [Hole.Pair 30 28, Hole.Pair 30 (28 - 80)] } bsl

def warden13 (bsl : Bsl) : Template :=
  placeToeHeel wardenToe sth2Heel bsl

def royal (bsl : Bsl) : Template :=
  placeToeHeel royalToe { holes := [Hole.Pair 32 25, Hole.Pair 32 (25 - 80)] } bsl

def royalToe : Template :=
  { holes := [Hole.Pair 36 (-12 + 31), Hole.Pair 36 (-12)] }

def xcompHeel : Template :=
  { holes := [Hole.Pair 20 12, Hole.Pair 40.5 (12 - 80)] }

def xcomp (bsl : Bsl) : Template :=
  placeToeHeel { holes := [Hole.Pair 36 (-12), Hole.Center (-12 - 55)] } xcompHeel bsl

def xcell (bsl : Bsl) : Template :=
  placeToeHeel royalToe xcompHeel bsl

def pistonPlate : Template :=
  { holes := [Hole.Pair 36 (122 + 66), Hole.Pair 36 (122 + 46), Hole.Pair 36 122, Hole.Pair 42 (-122), Hole.Pair 42 (-122 - 20), Hole.Pair 42 (-122 - 51)] }

def tyrolia (bsl : Bsl) : Template :=
  placeToeHeel { holes := [Hole.Pair 40 (-15 + 55), Hole.Pair 40 (-15)] } { holes := [Hole.Pair 20 17, Hole.Pair 43.25 (17 - 95)] } bsl

def tyroliaFreeflex (bsl : Bsl) : Template :=
  let innerToeHoles := -15
  let innerHeelHoles := innerToeHoles + 31.5 - 2.5 - nearest
  let nearest : Float :=
    let opts := List.range' 270 10 |-> fun i => 270 + i * 10  -- 270,280..360 but step 10? Wait, [270,280..360]
    let diffs := opts.map (fun opt => (Float.abs (bsl - opt.toFloat), opt.toFloat))
    (diffs.qsort (fun a b => a.fst < b.fst)).head!.snd
  Shift.shift (bsl / 2) { holes := [Hole.Pair 40 (innerToeHoles + 55), Hole.Pair 40 innerToeHoles, Hole.Pair 20 innerHeelHoles, Hole.Pair 43.25 (innerHeelHoles - 95)] }

def bmfNtn (bsl : Bsl) : Template :=
  Shift.shift (bsl / 2) { holes := [Hole.Pair 38 (-25), Hole.Pair 38 (-(25 + 38)), Hole.Pair 38 (-(25 + 38 + 38)), Hole.Pair 38 (-(25 + 38 + 38)), Hole.Center (-(25 + 38 + 38)), Hole.Center (-244), Hole.Center (-(244 + 38))] }

def templateLibrary : List (String × String × (Bsl → Template)) := [
  ("look-pivot", "Look Pivot", pivot),
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

def writeTemplate (file : System.FilePath) (name : String) (t : Template) : IO Unit :=
  IO.FS.writeFile file (svg (template name t))

def allTemplates : IO Unit := do
  for bslNat in List.range' 240 (111) do  -- 240 to 350 inclusive: 350-240+1=111
    let bsl := bslNat.toFloat
    for (name, desc, t) in templateLibrary do
      IO.FS.createDir name
      writeTemplate (name / s!"{name}-bsl-{bsl}.svg") (desc ++ s!", BSL: {bsl} mm") (t bsl)
  writeTemplate "look-r22.svg" "Look R22 Plate" r22
  writeTemplate "marker-piston-plate.svg" "Marker Piston Plate" pistonPlate
  writeTemplate "salomon-strive-demo.svg" "Salomon Strive Demo" striveDemo
  writeTemplate "tyrolia-power-rail.svg" "Tyrolia PowerRail (PR)" tyroliaPowerRail
  writeTemplate "tyrolia-super-lite-rail-xm.svg" "Tyrolia SuperLiteRail XM (SLR)" tyroliaSuperLiteRailXm
  writeTemplate "tyrolia-super-lite-rail-xl.svg" "Tyrolia SuperLiteRail XL (SLR)" tyroliaSuperLiteRailXl
  writeTemplate "tyrolia-attack-demo.svg" "Tyrolia Attack Demo" tyroliaAttackDemo
  for euroSize in List.range' 36 (15) do  -- 36 to 50: 15 items
    IO.FS.createDir "rossignol-ifp"
    let file := "rossignol-ifp" / s!"rossignol-ifp-euro-{euroSize}.svg"
    let temp := template (s!"Rossignol IFP, Euro Size: {euroSize}") (rossignolIFP euroSize)
    IO.FS.writeFile file (svg temp)

def holeDistance (a b : Hole) : Float :=
  match a, b with
  | Hole.Center a', Hole.Center b' => Float.abs (a' - b')
  | Hole.Pair aW aP, Hole.Pair bW bP => Float.sqrt ((aW / 2 - bW / 2) ^ 2 + (aP - bP) ^ 2)
  | Hole.Center aP, Hole.Pair bW bP => Float.sqrt ((bW / 2) ^ 2 + (aP - bP) ^ 2)
  | Hole.Pair aW aP, Hole.Center bP => Float.sqrt ((aW / 2) ^ 2 + (aP - bP) ^ 2)

def minimumHoleSpacing (t : Template) : Float :=
  let pairs := t.holes.bind fun a => t.holes.map fun b => (a, b)
  let dists := pairs.filterMap fun (a, b) => if a != b then some (holeDistance a b) else none
  dists.foldl Float.min Float.infinity

def checkRemount : IO Unit := do
  IO.println s!"Minimum hole distance: {minimumHoleSpacing (tyroliaPowerRail ++ pistonPlate)}"

end BindingTemplates

-/
