def Point : Type := Float × Float

inductive LineType where
  | solid
  | dashed

inductive Drawing where
  | empty : Drawing
  | compose : Drawing → Drawing → Drawing
  | line : LineType → Point → Point → Drawing
  | circle : Point → Float → Drawing
  | text : Nat → String → Drawing
  | translate : Point → Drawing → Drawing
  | rotate : Float → Point → Drawing → Drawing
  | scale : Point → Drawing → Drawing

instance : Append Drawing where
  append := Drawing.compose

instance : Inhabited Drawing where
  default := Drawing.empty

def line := Drawing.line LineType.solid
def dashedLine := Drawing.line LineType.dashed
def circle := Drawing.circle
def translate := Drawing.translate
def rotate := Drawing.rotate
def scale := Drawing.scale

def text (s : Nat) (t : String) :=
   scale (1, -1) (Drawing.text s t)



def target : Point → Drawing
  | (x, y) =>
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

-- Page parameters.
namespace Page
  def width : Float := 215.9
  def height : Float := 279.4
  /-
  def baseToe : Nat := 255
  def baseHeel : Nat := 5
  def centerHeel : Nat := 48 -- 1/4 of the page width.
  def center : Nat := centerHeel * 2
  def centerToe : Nat := centerHeel + center
  def hCenter : Nat := (baseToe - baseHeel) / 2 + baseHeel
  -/
end Page

/-
-- Draws a target on either the toe or heel template depending on the y-coordinate.
def target' : Point → Drawing
  | (x, y) => target
     (if y >= 0 then
       (Page.centerToe.toFloat + x, Page.baseToe.toFloat - y) else
       (Page.centerHeel.toFloat + x, Page.baseHeel.toFloat - y)
     )

-- Draw a hole or group of holes.
def drawHoles : Holes → Drawing
  | Holes.center y => target' (0, y)
  | Holes.pair w y => target' (-w / 2, y) ++ target' (w / 2, y)
  | Holes.triple w y => target' (-w / 2, y) ++ target' (w / 2, y) ++ target' (0, y)

def insertFloat (n : Float) (lst : List Float) : List Float :=
  match lst with
  | [] => [n]
  | x :: xs =>
    if n <= x then
      n :: lst
    else
      x :: insertFloat n xs

def sortFloats (lst : List Float) : List Float :=
  List.foldl (flip insertFloat) [] lst

-- All the y coordinates in the template.
def ys : Template → List Float
  | Template.center y => [y]
  | Template.pair _ y => [y]
  | Template.holes t1 t2 => ys t1 ++ ys t2


-- Pairs of neighboring values in a list.
def neighbors {α : Type} : List α → List (α × α)
  | [] => []
  | [_] => []
  | x :: y :: xs => (x, y) :: neighbors (y :: xs)

def yDimensions (t : Template) : (List (Float × Float) × List (Float × Float)) :=
  let (toes, heels) := List.partition (· ≥ 0) (ys t)
  let toes1  := neighbors (List.map (Page.baseToe.toFloat - ·)  (0 :: (sortFloats toes)))
  let heels1 := neighbors (List.map (Page.baseHeel.toFloat + ·) (0 :: (sortFloats (List.map Float.abs heels))))
  (toes1, heels1)

def pairDimensions (t : TemplateType) : (List (Float × Float) × List (Float × Float)) :=
  match t with
  | Template.pair w y => if y >= 0 then ([(w, y)], []) else ([], [(w, y.abs)])
  | Template.center _ => ([], [])
  | Template.holes t1 t2 =>
    let (toes1, heels1) := pairDimensions t1
    let (toes2, heels2) := pairDimensions t2
    (toes1 ++ toes2, heels1 ++ heels2)

def formatFloat (f : Float) : String :=
  let i10 := (Float.round (f * 10)).toInt32.toInt
  let iInt := toString (i10 / 10)
  let iFrac := toString (Int32.mod i10 10)
  iInt ++ "." ++ iFrac

def drawMeasurementLineY (x yA yB : Float) : Drawing :=
  let (y1, y2) := if yA < yB then (yA, yB) else (yB, yA)

  line (x, y1) (x, y2) ++

  -- Primary measurement mark, y1.
  line (x - 2.5, y1) (x + 2.5, y1) ++
  -- Arrow mark.
  line (x, y1) (x - 2.5, y1 + 2.5) ++
  line (x, y1) (x + 2.5, y1 + 2.5) ++

  -- Primary measurement mark, y2.
  line (x - 2.5, y2) (x + 2.5, y2) ++
  -- Arrow mark.
  line (x, y2) (x - 2.5, y2 - 2.5) ++
  line (x, y2) (x + 2.5, y2 - 2.5) ++

  text 4 (x + 2.5, (y2 - y1) / 2 + y1) (formatFloat (y2 - y1) ++ " mm")

def drawMeasurementLineX (a : Bool) (y xA xB : Float) : Drawing :=
  let (x1, x2) := if xA < xB then (xA, xB) else (xB, xA)

  line (x1, y) (x2, y) ++

  -- Primary measurement mark, y1.
  line (x1, y - 2.5) (x1, y + 2.5) ++
  -- Arrow mark.
  line (x1, y) (x1 + 2.5, y + 2.5) ++
  line (x1, y) (x1 + 2.5, y - 2.5) ++

  -- Primary measurement mark, y2.
  line (x2, y - 2.5) (x2, y + 2.5) ++
  -- Arrow mark.
  line (x2, y) (x2 - 2.5, y + 2.5) ++
  line (x2, y) (x2 - 2.5, y - 2.5) ++

  text 4 ((x2 - x1) / 2 + x1 + 5, if a then y + 10 else y - 10) (formatFloat (x2 - x1) ++ " mm")



def drawMeasurementLinesY (a : Bool) (x : Float) (ys : List (Float × Float)) : Drawing :=
  let xMark := if a then x - 5 else x + 5
  match ys with
  | [] => []
  | (y1, y2) :: rest =>
     drawMeasurementLineY xMark y1 y2 ++
     drawMeasurementLinesY (not a) x rest

def drawMeasurementsY (t : TemplateType) : Drawing :=
  let (toes, heels) := yDimensions t
  drawMeasurementLinesY true (Page.centerToe.toFloat - 35) toes ++
  drawMeasurementLinesY true (Page.centerHeel.toFloat - 35) heels

def drawMeasurementsX (t : TemplateType) : Drawing :=
  let (toes, heels) := pairDimensions t
  List.flatten (toes.map (λ (w, y) => drawMeasurementLineX false
    (Page.baseToe.toFloat - y - 10)
    (Page.centerToe.toFloat - w / 2)
    (Page.centerToe.toFloat + w / 2))) ++
  List.flatten (heels.map (λ (w, y) => drawMeasurementLineX true
    (Page.baseHeel.toFloat + y + 10)
    (Page.centerHeel.toFloat - w / 2)
    (Page.centerHeel.toFloat + w / 2)))

def drawMeasurements (t : TemplateType) : Drawing :=
  drawMeasurementsY t ++
  drawMeasurementsX t

-- Convert a Template to a Drawing.
def drawTemplate (t : TemplateType) : Drawing :=
  drawMeasurements t ++
  drawHoles t ++
  text 4 (Page.centerHeel.toFloat + 25, Page.baseHeel.toFloat + 20) "Mid Sole Mount Line" ++
  text 4 (Page.centerToe.toFloat  + 25, Page.baseToe.toFloat  - 20) "Mid Sole Mount Line" ++
  text 4 (Page.centerHeel.toFloat - 5, Page.baseHeel.toFloat + 230) "Ski Center Line" ++
  text 4 (Page.centerToe.toFloat  - 5, Page.baseToe.toFloat  - 230) "Ski Center Line"

-- Draws X and Y centering lines for the template,
-- which align with the mid sole mark and the mid line on the skis.
def centerLines : Drawing :=
  List.flatten
    -- Center lines.
    [ line (Page.centerHeel.toFloat, 0) (Page.centerHeel.toFloat, Page.height),
      line (Page.centerToe.toFloat, 0) (Page.centerToe.toFloat, Page.height),
      -- Mount point lines.
      line (Page.centerToe.toFloat - 40, Page.baseToe.toFloat) (Page.centerToe.toFloat + 40, Page.baseToe.toFloat),
      line (Page.centerHeel.toFloat - 40, Page.baseHeel.toFloat) (Page.centerHeel.toFloat + 40, Page.baseHeel.toFloat),
      -- Center trim line.
      dashedLine (Page.center.toFloat, 0) (Page.center.toFloat, Page.height),
      -- End trim lines.
      dashedLine (0, Page.height) (Page.center.toFloat, Page.height),
      dashedLine (Page.center.toFloat, 0) (Page.width, 0),
      -- Notch trim lines.
      dashedLine (Page.centerHeel.toFloat, Page.baseHeel.toFloat + 2) (Page.centerHeel.toFloat - 7, 0),
      dashedLine (Page.centerHeel.toFloat, Page.baseHeel.toFloat + 2) (Page.centerHeel.toFloat + 7, 0),
      dashedLine (Page.centerToe.toFloat, Page.baseToe.toFloat - 2) (Page.centerToe.toFloat - 6, Page.height),
      dashedLine (Page.centerToe.toFloat, Page.baseToe.toFloat - 2) (Page.centerToe.toFloat + 6, Page.height)
    ]

-/
