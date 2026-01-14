import BindingTemplates.Templates

def Drawing : Type := List String

instance : Append Drawing where
  append := List.append

def drawingToStrings (d : Drawing) :  List String := d

def tag' (isOpen : Bool) (element : String) (attrs : List (String × String)) : String :=
  "<" ++ element ++
   String.join (List.map (λ attr => " " ++ attr.fst ++ "=\"" ++ attr.snd ++ "\"") attrs) ++
   (if isOpen then " >" else " />")

def tag : String → List (String × String) → String :=
  tag' false

def tagOpen : String → List (String × String) → String :=
  tag' true

def tagClose (element : String) : String :=
  "</" ++ element ++ ">"

def Point : Type := Float × Float

def text : Nat → Point → String → Drawing
  | size, (x, y), msg =>
    [ tagOpen "text"
        [ ("x", toString x),
          ("y", toString y),
          ("font-family", "Arial"),
          ("font-size", toString size),
          ("text-anchor", "middle"),
          ("dominant-baseline", "middle"),
          ("transform", "rotate(90 " ++ toString x ++ " " ++ toString y ++ ")")
        ] ++
      msg ++
      tagClose "text"
    ]

def line : Point → Point → Drawing
  | (x1, y1), (x2, y2) =>
    [ tag "line"
      [ ("x1", toString x1),
        ("y1", toString y1),
        ("x2", toString x2),
        ("y2", toString y2),
        ("stroke", "black"),
        ("stroke-width", "0.1")
      ]
    ]

def dashedLine : Point → Point → Drawing
  | (x1, y1), (x2, y2) =>
    [ tag "line"
      [ ("x1", toString x1),
        ("y1", toString y1),
        ("x2", toString x2),
        ("y2", toString y2),
        ("stroke", "black"),
        ("stroke-width", "0.1"),
        ("stroke-dasharray", "2 2")
      ]
    ]

def circle : Point → Float → Drawing
  | (cx, cy), r =>
    [ tag "circle"
      [ ("cx", toString cx),
        ("cy", toString cy),
        ("r", toString r),
        ("fill", "none"),
        ("stroke", "black"),
        ("stroke-width", "0.1")
      ]
    ]

def target : Point → Drawing
  | (x, y) => List.flatten
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

-- Page parameters.
namespace Page
  def width : Float := 192
  def height : Float := 259
  def center1 : Nat := 48  -- 1/4 of the page width, 48
  def center : Nat := center1 * 2
  def center2 : Nat := center1 + center
  def baseToe : Nat := 255
  def baseHeel : Nat := 5
  def centerToe : Nat := center2
  def centerHeel : Nat := center1
  def hCenter : Nat := (baseToe - baseHeel) / 2 + baseHeel
end Page

-- Draws a target on either the toe or heel template depending on the y-coordinate.
def target' : Point → Drawing
  | (x, y) => target
     (if y >= 0 then
       (Page.center2.toFloat + x, Page.baseToe.toFloat - y) else
       (Page.center1.toFloat + x, Page.baseHeel.toFloat - y)
     )

-- Convert a Template to a Drawing.
def drawHoles : Template → Drawing
  | Template.center y => target' (0, y)
  | Template.pair w y => target' (-w / 2, y) ++ target' (w / 2, y)
  | Template.holes t1 t2 => drawHoles t1 ++ drawHoles t2

/-
-- Merges two sorted lists of floats into one sorted list.
def mergeSortFloats4 (lst1 lst2 : List Float) : List Float :=
  match (lst1, lst2) with
  | ([], _) => lst2
  | (_, []) => lst1
  | (x1 :: xs1, x2 :: xs2) =>
    if (x1 < x2) then
      x1 :: mergeSortFloats4 xs1 lst2
    else if x1 > x2 then
      x2 :: mergeSortFloats4 lst1 xs2
    else
      x1 :: mergeSortFloats4 xs1 xs2
  termination_by lst1.length + lst2.length

-- Reduce a list of lists of sorted floats by merging neighboring pairs.
def mergeSortFloats2 (lsts : List (List Float)) : List (List Float) :=
  match lsts with
  | [] => []
  | [lst] => [lst]
  | lst1 :: lst2 :: rest => mergeSortFloats4 lst1 lst2 :: mergeSortFloats2 rest

-- Fixed point of mergeSortFloats2.
def mergeSortFloats1 (lsts : List (List Float)) : List Float :=
  match lsts with
  | [] => sorry
  | [lst] => lst
  | _ => mergeSortFloats1 (mergeSortFloats2 lsts)

-- Merge sort of a list of floats.
def mergeSortFloats (lst : List Float) : List Float :=
  mergeSortFloats1 (lst.map (· :: []))
-/

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

def pairDimensions (t : Template) : (List (Float × Float) × List (Float × Float)) :=
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

def drawMeasurementsY (t : Template) : Drawing :=
  let (toes, heels) := yDimensions t
  drawMeasurementLinesY true (Page.centerToe.toFloat - 35) toes ++
  drawMeasurementLinesY true (Page.centerHeel.toFloat - 35) heels

def drawMeasurementsX (t : Template) : Drawing :=
  let (toes, heels) := pairDimensions t
  List.flatten (toes.map (λ (w, y) => drawMeasurementLineX false
    (Page.baseToe.toFloat - y - 10)
    (Page.centerToe.toFloat - w / 2)
    (Page.centerToe.toFloat + w / 2))) ++
  List.flatten (heels.map (λ (w, y) => drawMeasurementLineX true
    (Page.baseHeel.toFloat + y + 10)
    (Page.centerHeel.toFloat - w / 2)
    (Page.centerHeel.toFloat + w / 2)))

def drawMeasurements (t : Template) : Drawing :=
  drawMeasurementsY t ++
  drawMeasurementsX t

-- Convert a Template to a Drawing.
def drawTemplate (t : Template) : Drawing :=
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
    [ line (Page.center1.toFloat, 0) (Page.center1.toFloat, Page.height),
      line (Page.center2.toFloat, 0) (Page.center2.toFloat, Page.height),
      -- Mount point lines.
      line (Page.center2.toFloat - 40, Page.baseToe.toFloat) (Page.center2.toFloat + 40, Page.baseToe.toFloat),
      line (Page.center1.toFloat - 40, Page.baseHeel.toFloat) (Page.center1.toFloat + 40, Page.baseHeel.toFloat),
      -- Center trim line.
      dashedLine (Page.center.toFloat, 0) (Page.center.toFloat, Page.height),
      -- End trim lines.
      dashedLine (0, Page.height) (Page.center.toFloat, Page.height),
      dashedLine (Page.center.toFloat, 0) (Page.width, 0),
      -- Notch trim lines.
      dashedLine (Page.center1.toFloat, Page.baseHeel.toFloat + 2) (Page.center1.toFloat - 7, 0),
      dashedLine (Page.center1.toFloat, Page.baseHeel.toFloat + 2) (Page.center1.toFloat + 7, 0),
      dashedLine (Page.center2.toFloat, Page.baseToe.toFloat - 2) (Page.center2.toFloat - 6, Page.height),
      dashedLine (Page.center2.toFloat, Page.baseToe.toFloat - 2) (Page.center2.toFloat + 6, Page.height)
    ]

namespace ScalingRulers

  def width (n : Float) : Float :=
    let nInt := n.round.toInt32.toInt
    if nInt % 10 = 0 then
      3
    else if nInt % 5 = 0 then
      2
    else
      1

  def vertical (y : Nat) : Drawing :=
    line (0, y.toFloat) (width $ y.toFloat - 5, y.toFloat)

  def horizontal (x : Nat) : Drawing :=
    line (x.toFloat, Page.baseToe.toFloat) (x.toFloat, Page.baseToe.toFloat - width (x.toFloat - Page.center1.toFloat))

  def scalingRulers : Drawing :=
    List.flatten (vertical <$> List.range' Page.baseHeel (Page.baseToe - Page.baseHeel + 1) 1) ++
    List.flatten (horizontal <$> (List.range' (Page.center1 - 40) 41 1 ++ List.range' Page.center1 41 1))

end ScalingRulers

-- | Converts Drawing to an SVG file.
def svg (name : String) (t : Template) : String :=
  String.intercalate "\n" (
    [ "<?xml version=\"1.0\"?>",
      "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"",
      "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">",
      tagOpen "svg"
        [ ("xmlns", "http://www.w3.org/2000/svg"),
          ("version", "1.2"),
          ("width", toString Page.width ++ "mm"),
          ("height", toString Page.height ++ "mm"),
          ("viewBox", "0 0 " ++ toString Page.width ++ " " ++ toString Page.height),
          ("style", "background-color:white")
        ]
    ] ++
    drawingToStrings (
      centerLines ++
      -- ScalingRulers.scalingRulers ++
      text 8 (Page.centerHeel.toFloat + 35, Page.hCenter.toFloat) (name ++ ", Heel") ++
      text 8 (Page.centerToe.toFloat  + 35, Page.hCenter.toFloat) (name ++ ", Toe") ++
      drawTemplate t ) ++
    [tagClose "svg"]
  )
