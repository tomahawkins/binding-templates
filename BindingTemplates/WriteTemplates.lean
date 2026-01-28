import BindingTemplates.Drawing
import BindingTemplates.Svg
import BindingTemplates.Templates

-- Toe or heel page or section.
inductive Binding where
  | toe
  | heel

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

-- For toe.  Flip for heel.
def trimMountLine : Drawing :=
  dashedLine (0, 2.5) (10, -7.5) ++
  dashedLine (0, 2.5) (-10, -7.5)

def mountLine (b : Binding) : Drawing :=
  translate (55, 0) (text 4 "Mount Line") ++
  line (-40, 0) (40, 0) ++
  -- Dashed lines for trimming.
  match b with
  | Binding.toe => trimMountLine
  | Binding.heel => scale (1, -1) trimMountLine

def bootLine (b : Binding) : Drawing :=
  let msg := match b with
    | Binding.toe => "Boot Toe Edge"
    | Binding.heel => "Boot Heel Edge"
  translate (45, 0) (text 4 msg) ++
  dashedLine (-20, 0) (20, 0)

def skiCenterLine : Binding → Drawing
  | Binding.toe =>
      translate (20, 250) (text 4 "Ski Center Line") ++
      line (0, 0) (0, 300)
  | Binding.heel =>
      translate (20, -250) (text 4 "Ski Center Line") ++
      line (0, 0) (0, -300)

def drawTemplateRegularToe (descr : String) (holes : List Holes) (bsl : Option Nat) : Drawing :=
  -- Toe binding.
  translate (0, - Page.height / 2 + 10) (
    translate (-80, 250) (text 8 descr) ++
    translate (-80, 240) (text 8 "Toe") ++
    (match bsl with
     | none => Drawing.empty
     | some n => translate (-80, 230) (text 8 ("BSL: " ++ toString n))
    ) ++
    mountLine Binding.toe ++
    -- Ski center line.
    skiCenterLine Binding.toe ++
    -- Toe binding.
    match bsl with
    | none => concatMap drawHoles holes
    | some bsl => translate (0, bsl.toFloat / 2) (bootLine Binding.toe ++ concatMap drawHoles holes)
  )

def drawTemplateRegularHeel (holes : List Holes) (bsl : Option Nat) : Drawing :=
  -- Heel binding.
  translate (0, Page.height / 2 - 10) (
    translate (-80, -240) (text 8 "Heel") ++
    mountLine Binding.heel ++
    -- Ski center line.
    skiCenterLine Binding.heel ++
    -- Toe binding.
    match bsl with
    | none => concatMap drawHoles holes
    | some bsl => translate (0, - bsl.toFloat / 2) (bootLine Binding.heel ++ concatMap drawHoles holes)
  )

def coordinatesSvg (d : Drawing) : Drawing :=
  translate (Page.width / 2, Page.height / 2) (scale (1, -1) d)

def writeTemplateRegular (company name descr : String) (toeHoles heelHoles : List Holes) (bsl : Nat) : IO Unit := do
  IO.println ("  Writing " ++ company ++ " " ++ descr ++ " " ++ toString bsl ++ "...")
  let d1 := "generated-templates"
  let d2 := d1 ++ "/" ++ company
  let d3 := d2 ++ "/" ++ name
  let f := d3 ++ "/" ++ name ++ "-" ++ toString bsl
  createDir d1
  createDir d2
  createDir d3
  pdf f
    [ coordinatesSvg (drawTemplateRegularToe  descr toeHoles (Option.some bsl)),
      coordinatesSvg (drawTemplateRegularHeel heelHoles (Option.some bsl)),
    ]

def writeTemplateCustom (company name descr : String) (holes : Float → List Holes) (bsl : Nat) : IO Unit := do
  IO.println ("  Writing " ++ company ++ " " ++ descr ++ " " ++ toString bsl ++ "...")
  let d1 := "generated-templates"
  let d2 := d1 ++ "/" ++ company
  let d3 := d2 ++ "/" ++ name
  let f := d3 ++ "/" ++ name ++ "-" ++ toString bsl
  createDir d1
  createDir d2
  createDir d3
  let (frontHoles, backHoles) := partitionHoles (holes bsl.toFloat)
  let frontHolesShifted := frontHoles.map (shiftHoles (- bsl.toFloat / 2))
  let backHolesShifted := backHoles.map (shiftHoles (bsl.toFloat / 2))
  pdf f
    [ coordinatesSvg (drawTemplateRegularToe  descr frontHolesShifted (Option.some bsl)),
      coordinatesSvg (drawTemplateRegularHeel backHolesShifted (Option.some bsl)),
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
    [ coordinatesSvg (drawTemplateRegularToe descr frontHoles Option.none),
      coordinatesSvg (drawTemplateRegularHeel backHoles Option.none)
    ]

def bslRange : List Nat :=
  List.range' 240 (350 - 240 + 1) 1

def writeTemplate (t : Template) : IO Unit :=
  match t.template with
  | TemplateType.regular toe heel => do
      let _ ← bslRange.mapM (writeTemplateRegular t.company t.file t.description toe heel)
  | TemplateType.custom f => do
      let _ ← bslRange.mapM (writeTemplateCustom t.company t.file t.description f)
  | TemplateType.plate holes => writeTemplatePlate t.company t.file t.description holes

def writeTemplates : IO Unit := do
  IO.println "Writing templates..."
  let _ ← templates.mapM writeTemplate
  pure ()
