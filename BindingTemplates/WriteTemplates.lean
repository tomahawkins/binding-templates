import BindingTemplates.Drawing
import BindingTemplates.Templates

def createDir (d : System.FilePath) : IO Unit := do
  let dirExists ← System.FilePath.isDir d
  if dirExists then
    pure ()
  else
    IO.FS.createDir d

def writePlateTemplate : (String × String × String × Template) → IO Unit
  | (company, product, description, t) => do
    let d1 := "generated-templates"
    let d2 := d1 ++ "/" ++ company
    let file := d2 ++ "/" ++ product ++ ".svg"
    createDir d1
    createDir d2
    IO.FS.writeFile file (svg (company ++ " " ++ description) t)

def plateTemplates : List (String × String × String × Template) :=
  [ ("Look",    "R22",             "R22",                    Look.r22),
    ("Marker",  "PistonPlate",     "Piston Plate",           Marker.pistonPlate),
    ("Salomon", "StriveDemo",      "Strive Demo",            Salomon.striveDemo),
    ("Tyrolia", "PowerRail",       "PowerRail (PR)",         Tyrolia.powerRail),
    ("Tyrolia", "SuperLiteRailXM", "SuperLiteRail XM (SLR)", Tyrolia.superLiteRailXm),
    ("Tyrolia", "SuperLiteRailXL", "SuperLiteRail XL (SLR)", Tyrolia.superLiteRailXl),
    ("Tyrolia", "AttackDemo",      "Attack Demo",            Tyrolia.attackDemo)
  ]

def writeBslTemplate : (String × String × String × BslTemplate) → Nat → IO Unit
  | (company, product, description, t), bsl => do
    let d1 := "generated-templates"
    let d2 := d1 ++ "/" ++ company
    let d3 := d2 ++ "/" ++ product
    let file := d3 ++ "/" ++ product ++ "-" ++ toString bsl ++ ".svg"
    createDir d1
    createDir d2
    createDir d3
    IO.FS.writeFile file (svg (company ++ " " ++ description ++ " " ++ toString bsl) (t bsl.toFloat))

def bslTemplates : List (String × String × String × BslTemplate) :=
  [ ("Look",    "Pivot",      "Pivot",                   Look.pivot),
    ("Look",    "SPX",        "SPX",                     Look.spx),
    ("Look",    "Rockerace",  "Rockerace",               Look.rockerace),

    ("Salomon", "Shift",      "Shift",                   Salomon.shift),
    ("Salomon", "STH2",       "STH2",                    Salomon.sth2),
    ("Salomon", "Warden-11",  "Warden 11, Strive 12/14", Salomon.warden11),
    ("Salomon", "Warden-13",  "Warden 13, Strive 16",    Salomon.warden13),

    ("Marker", "Royal", "Royal (Jester, Griffon, etc)",  Marker.royal),
    ("Marker", "XComp", "XComp",                         Marker.xcomp),
    ("Marker", "XCell", "XCell",                         Marker.xcell),

    ("Tyrolia", "Common",     "Tyrolia Common",  Tyrolia.tyrolia),
    ("Tyrolia", "FreeFlexST", "FreeFlex ST",     Tyrolia.freeflex),

    ("Bishop", "BMF-NTN", "BMF NTN", Bishop.bmfNtn)

  ]

def bslRange : List Nat :=
  List.range' 240 (350 - 240 + 1) 1

def writeBslTemplates : (String × String × String × BslTemplate) → IO Unit
  | (company, product, description, t) => do
    let _ ← List.mapM (writeBslTemplate (company, product, description, t)) bslRange

def writeTemplates : IO Unit := do
  IO.println "Writing templates..."
  let _ ← List.mapM writePlateTemplate plateTemplates
  let _ ← List.mapM writeBslTemplates bslTemplates
