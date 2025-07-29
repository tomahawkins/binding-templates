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

def text : Point → String → Drawing
  | (x, y), msg =>
    [ tagOpen "text"
        [ ("x", toString x),
          ("y", toString y),
          ("class", "small"),
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
end Page

-- Draws a target on either the toe or heel template depending on the y-coordinate.
def target' : Point → Drawing
  | (x, y) => target
     (if y >= 0 then
       (Page.center2.toFloat + x, Page.baseToe.toFloat - y) else
       (Page.center1.toFloat + x, Page.baseHeel.toFloat - y)
     )

def drawTemplate : Template → Drawing
  | Template.hole x y => target' (x, y)
  | Template.holes t1 t2 => drawTemplate t1 ++ drawTemplate t2

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
        ],
      tagOpen "style" [],
      ".small { font: 6px sans-serif; }",
      tagClose "style"
    ] ++
    drawingToStrings (
      centerLines ++
      ScalingRulers.scalingRulers ++
      text (Page.center1.toFloat + 35, Page.baseHeel.toFloat + 5) (name ++ ", Heel") ++
      text (Page.center2.toFloat + 35, Page.baseHeel.toFloat + 5) (name ++ ", Toe") ++
      drawTemplate t ) ++
    [tagClose "svg"]
  )
