import BindingTemplates.Drawing

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

def indent (lines : List String) : List String :=
  lines.map ("  " ++ ·)

def renderDrawing (d : Drawing) : List String :=
  match d with
  | Drawing.empty => []
  | Drawing.compose d1 d2 => renderDrawing d1 ++ renderDrawing d2
  | Drawing.line lineType (x1, y1) (x2, y2) =>
      let style :=
        match lineType with
        | LineType.solid => []
        | LineType.dashed => [("stroke-dasharray", "2 2")]
      [ tag "line"
          ([ ("x1", toString x1),
            ("y1", toString y1),
            ("x2", toString x2),
            ("y2", toString y2),
            ("stroke", "black"),
            ("stroke-width", "0.1"),
          ] ++ style)
      ]
  | Drawing.circle (cx, cy) r =>
      [ tag "circle"
          [ ("cx", toString cx),
            ("cy", toString cy),
            ("r", toString r),
            ("fill", "none"),
            ("stroke", "black"),
            ("stroke-width", "0.1")
          ]
      ]
  | Drawing.translate (dx, dy) d' =>
      [tagOpen "g" [("transform", "translate(" ++ toString dx ++ " " ++ toString dy ++ ")")]] ++
      indent (renderDrawing d') ++
      [tagClose "g"]
  | Drawing.rotate angle (x, y) d' =>
      [tagOpen "g" [("transform", "rotate(" ++ toString angle ++ " " ++ toString x ++ " " ++ toString y ++ ")")]] ++
      indent (renderDrawing d') ++
      [tagClose "g"]
  | Drawing.scale (x, y) d' =>
      [tagOpen "g" [("transform", "scale(" ++ toString x ++ " " ++ toString y ++ ")")]] ++
      indent (renderDrawing d') ++
      [tagClose "g"]
  | Drawing.text size msg =>
      [ tagOpen "text"
          [ ("x", "0"),
            ("y", "0"),
            ("font-family", "Arial"),
            ("font-size", toString size),
            ("text-anchor", "middle"),
            ("dominant-baseline", "middle"),
            -- ("transform", "rotate(90 0 0)")
          ] ++
        msg ++
        tagClose "text"
      ]

-- Convert a Drawing to an SVG file sized to US letter.
def svg (d : Drawing) : String :=
  String.intercalate "\n" (
    [ tagOpen "svg"
        [ ("width", toString Page.width ++ "mm"),
          ("height", toString Page.height ++ "mm"),
          ("viewBox", "0 0 " ++ toString Page.width ++ " " ++ toString Page.height),
        ]
    ] ++
    indent (renderDrawing d) ++
    [tagClose "svg"]
  )

-- Use Inkscape to convert svg to pdf.
def pdfPage (file : System.FilePath) (d : Drawing × Nat) : IO Unit := do
  let (d, n) := d
  let svgName := toString file ++ "-" ++ toString n ++ ".svg"
  let pdfName := toString file ++ "-" ++ toString n ++ ".pdf"
  IO.FS.writeFile svgName (svg d)
  let _ ← IO.Process.output
    { cmd := "rsvg-convert",
      args := #["--format", "pdf1.7", "--output", pdfName, svgName],
      cwd := none,
      env := #[],
      inheritEnv := true,
      setsid := false,
    }
  IO.FS.removeFile svgName

-- Combine multiple PDF pages into a single PDF.
def pdfUnite (file : System.FilePath) (n : Nat) : IO Unit := do
  let pages : List String := (List.range n).map (λ n => toString file ++ "-" ++ toString n ++ ".pdf")
  let _ ← IO.Process.output
    { cmd := "pdfunite",
      args := List.toArray (pages ++ [toString file ++ ".pdf"]),
      cwd := none,
      env := #[],
      inheritEnv := true,
      setsid := false,
    }
  let _ ← pages.mapM (λ f => IO.FS.removeFile (System.FilePath.mk f))

-- Convert a list of drawings to a PDF file.
def pdf (file : System.FilePath) (drawings : List Drawing) : IO Unit := do
  let n := drawings.length
  let _ ← (drawings.zip (List.range n)).mapM (pdfPage file)
  pdfUnite file n
