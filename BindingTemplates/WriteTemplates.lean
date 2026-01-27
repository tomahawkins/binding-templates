import BindingTemplates.Drawing
import BindingTemplates.Svg
import BindingTemplates.Templates

def createDir (d : System.FilePath) : IO Unit := do
  let dirExists ← System.FilePath.isDir d
  if dirExists then
    pure ()
  else
    IO.FS.createDir d

def drawHoles (h : Holes) : Drawing :=
  match h with
  | Holes.center y => target (0, y)
  | Holes.pair w y => target (-w / 2, y) ++ target (w / 2, y)
  | Holes.triple w y => target (-w / 2, y) ++ target (w / 2, y) ++ target (0, y)

def concat {a : Type} [Append a] [Inhabited a] (lst : List a) : a :=
  List.foldl Append.append default lst

def concatMap (f : a → Drawing) (b : List a) :=
  concat (b.map f)

def mountLine : Drawing :=
  translate (55, 0) (text 4 "Mount Line") ++
  line (-40, 0) (40, 0)

def bootLine : Drawing :=
  dashedLine (-20, 0) (20, 0)

inductive Binding where
  | toe
  | heel

def skiCenterLine : Binding → Drawing
  | Binding.toe =>
      translate (20, 250) (text 4 "Ski Center Line") ++
      line (0, 0) (0, 300)
  | Binding.heel =>
      translate (20, -250) (text 4 "Ski Center Line") ++
      line (0, 0) (0, -300)

def drawTemplateRegularToe (descr : String) (toe : List Holes) (bsl : Nat) : Drawing :=
  -- Toe binding.
  translate (0, - Page.height / 2 + 10) (
    translate (-80, 250) (text 8 descr) ++
    translate (-80, 240) (text 8 "Toe") ++
    (if bsl = 0 then Drawing.empty else translate (-80, 230) (text 8 ("BSL: " ++ toString bsl))) ++
    mountLine ++
    -- Ski center line.
    skiCenterLine Binding.toe ++
    -- Toe binding.
    translate (0, bsl.toFloat / 2) (bootLine ++ concatMap drawHoles toe)
  )

def drawTemplateRegularHeel (heel : List Holes) (bsl : Nat) : Drawing :=
  -- Heel binding.
  translate (0, Page.height / 2 - 10) (
    translate (-80, -240) (text 8 "Heel") ++
    mountLine ++
    -- Ski center line.
    skiCenterLine Binding.heel ++
    -- Toe binding.
    translate (0, - bsl.toFloat / 2) (bootLine ++ concatMap drawHoles heel)
  )

def coordinatesSvg (d : Drawing) : Drawing :=
  translate (Page.width / 2, Page.height / 2) (scale (1, -1) d)

def writeTemplateRegular (company name descr : String) (toe heel : List Holes) (bsl : Nat) : IO Unit := do
  IO.println ("  Writing " ++ company ++ " " ++ descr ++ " " ++ toString bsl ++ "...")
  let d1 := "generated-templates"
  let d2 := d1 ++ "/" ++ company
  let d3 := d2 ++ "/" ++ name
  let f := d3 ++ "/" ++ name ++ "-" ++ toString bsl
  createDir d1
  createDir d2
  createDir d3
  pdf f
    [ coordinatesSvg (drawTemplateRegularToe  descr toe  bsl),
      coordinatesSvg (drawTemplateRegularHeel heel bsl),
    ]

def writeTemplatePlate (company name descr : String) (holes : List Holes) : IO Unit := do
  IO.println ("  Writing " ++ company ++ " " ++ descr ++ " " ++ "...")
  let d1 := "generated-templates"
  let d2 := d1 ++ "/" ++ company
  let f := d2 ++ "/" ++ name
  createDir d1
  createDir d2
  let (frontHoles, backHoles) := partitionHoles holes
  pdf f
    [ coordinatesSvg (drawTemplateRegularToe descr frontHoles 0),
      coordinatesSvg (drawTemplateRegularHeel backHoles 0)
    ]

def bslRange : List Nat :=
  List.range' 240 (350 - 240 + 1) 1

def writeTemplate (t : Template) : IO Unit :=
  match t.template with
  | TemplateType.regular toe heel => do
      let _ ← bslRange.mapM (writeTemplateRegular t.company t.file t.description toe heel)
      pure ()
  | TemplateType.plate holes => writeTemplatePlate t.company t.file t.description holes
  | TemplateType.custom _f => IO.print ("  Temporarily not generating template for custom stuff: " ++ t.description)

def writeTemplates : IO Unit := do
  IO.println "Writing templates..."
  let _ ← templates.mapM writeTemplate
  pure ()
