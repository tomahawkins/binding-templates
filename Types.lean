inductive Template where
  | hole : Float -> Float -> Template
  | holes : Template -> Template -> Template
deriving BEq

instance : Append Template where
  append := Template.holes

def center (y : Float) : Template :=
  Template.hole 0 y

def pair (width y : Float) : Template :=
  Template.holes (Template.hole (-width / 2) y) (Template.hole (width / 2) y)

#check center 22
#check pair 32 22
