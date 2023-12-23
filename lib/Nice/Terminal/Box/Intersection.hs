{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Nice.Terminal.Box.Intersection (
  Drawing,
  draw,

  -- * Known drawings
  up,
  down,
  left,
  right,
  horiz,
  vert,
  cornerBL,
  cornerBR,
  cornerTL,
  cornerTR,
  dashed,
  doubled,
  heavy,
) where

data Dir
  = DUp
  | DDown
  | DLeft
  | DRight
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Dashes = D2 | D3 | D4
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Weight = Light | Heavy
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data DirWeight = DW'None | DW'Light | DW'Heavy
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data LineDir = Horiz | Vert
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data DoubleDir = D'Horiz | D'Vert | D'Both
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Corner = TL | TR | BL | BR
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Drawing
  = Arc !Corner
  | DashedLine {lddir :: !LineDir, lddashes :: !Dashes, ldweight :: !Weight}
  | SolidDirs {sup, sdown, sleft, sright :: !DirWeight}
  | DoubleDirs {ddir :: !DoubleDir, dup, ddown, dleft, dright :: !Bool}
  | Blank
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Well-known drawings

up, down, left, right :: Drawing
up = SolidDirs DW'Light DW'None DW'None DW'None
down = SolidDirs DW'None DW'Light DW'None DW'None
left = SolidDirs DW'None DW'None DW'Light DW'None
right = SolidDirs DW'None DW'None DW'None DW'Light

horiz, vert :: Drawing
horiz = left <> right
vert = up <> down

cornerBL, cornerBR, cornerTL, cornerTR :: Drawing
cornerBL = up <> left
cornerBR = up <> right
cornerTL = down <> right
cornerTR = down <> left

dashed :: Dashes -> Weight -> Drawing -> Drawing
dashed d w (DashedLine dir _ _) = DashedLine dir d w
dashed d w dr | dr == horiz = DashedLine Horiz d w
dashed d w dr | dr == vert = DashedLine Vert d w
dashed _ _ dr = dr

doubled :: Drawing -> Drawing
doubled (DashedLine Horiz _ _) = DoubleDirs D'Horiz False False True True
doubled (DashedLine Vert _ _) = DoubleDirs D'Vert True True False False
doubled (SolidDirs u d l r) = DoubleDirs D'Both (u /= DW'None) (d /= DW'None) (l /= DW'None) (r /= DW'None)
doubled (DoubleDirs _ u d l r) = DoubleDirs D'Both u d l r
doubled dr = dr

heavy :: Drawing -> Drawing
heavy (DashedLine Horiz _ _) = SolidDirs DW'None DW'None DW'Heavy DW'Heavy
heavy (DashedLine Vert _ _) = SolidDirs DW'Heavy DW'Heavy DW'None DW'None
heavy (SolidDirs u d l r) = SolidDirs (asHeavy u) (asHeavy d) (asHeavy l) (asHeavy r)
  where
    asHeavy :: DirWeight -> DirWeight
    asHeavy DW'None = DW'None
    asHeavy _ = DW'Heavy
heavy dr = dr

--------------------------------------------------------------------------------

instance Monoid Drawing where
  mempty = Blank

instance Semigroup Drawing where
  (<>) Blank a = a
  (<>) a Blank = a
  DashedLine ld0 ls0 lw0 <> DashedLine ld1 ls1 lw1 = DashedLine (max ld0 ld1) (max ls0 ls1) (max lw0 lw1)
  SolidDirs u0 d0 l0 r0 <> SolidDirs u1 d1 l1 r1 = SolidDirs (max u0 u1) (max d0 d1) (max l0 l1) (max r0 r1)
  DoubleDirs dir0 u0 d0 l0 r0 <> DoubleDirs dir1 u1 d1 l1 r1 = DoubleDirs (max dir0 dir1) (max u0 u1) (max d0 d1) (max l0 l1) (max r0 r1)
  a <> b = max a b

solidIntersectFull :: Weight -> Drawing
solidIntersectFull Heavy = SolidDirs DW'Heavy DW'Heavy DW'Heavy DW'Heavy
solidIntersectFull Light = SolidDirs DW'Light DW'Light DW'Light DW'Light

ifHeavy :: Weight -> a -> a -> a
ifHeavy w a b = case w of
  Heavy -> a
  Light -> b

solidIntersectHV :: Weight -> Weight -> Drawing
solidIntersectHV h v = SolidDirs h' h' v' v'
  where
    h' = ifHeavy h DW'Heavy DW'Light
    v' = ifHeavy v DW'Heavy DW'Light

--------------------------------------------------------------------------------

draw :: Drawing -> Char
draw = \case
  Blank -> ' '
  Arc TL -> '╭'
  Arc TR -> '╮'
  Arc BR -> '╯'
  Arc BL -> '╰'
  DashedLine Horiz D2 Light -> '╌'
  DashedLine Horiz D2 Heavy -> '╍'
  DashedLine Horiz D3 Light -> '┄'
  DashedLine Horiz D3 Heavy -> '┅'
  DashedLine Horiz D4 Light -> '┈'
  DashedLine Horiz D4 Heavy -> '┉'
  DashedLine Vert D2 Light -> '╎'
  DashedLine Vert D2 Heavy -> '╏'
  DashedLine Vert D3 Light -> '┆'
  DashedLine Vert D3 Heavy -> '┇'
  DashedLine Vert D4 Light -> '┊'
  DashedLine Vert D4 Heavy -> '┋'
  SolidDirs DW'None DW'None DW'Light DW'None -> '╴'
  SolidDirs DW'None DW'None DW'Light DW'Light -> '─'
  SolidDirs DW'None DW'None DW'Heavy DW'Heavy -> '━'
  SolidDirs DW'Light DW'Light DW'None DW'None -> '│'
  SolidDirs DW'Heavy DW'Heavy DW'None DW'None -> '┃'
  SolidDirs DW'Light DW'None DW'None DW'None -> '╵'
  SolidDirs DW'None DW'None DW'None DW'Light -> '╶'
  SolidDirs DW'None DW'Light DW'None DW'None -> '╷'
  SolidDirs DW'None DW'None DW'Heavy DW'None -> '╸'
  SolidDirs DW'Heavy DW'None DW'None DW'None -> '╹'
  SolidDirs DW'None DW'None DW'None DW'Heavy -> '╺'
  SolidDirs DW'None DW'Heavy DW'None DW'None -> '╻'
  SolidDirs DW'None DW'None DW'Light DW'Heavy -> '╼'
  SolidDirs DW'Light DW'Heavy DW'None DW'None -> '╽'
  SolidDirs DW'None DW'None DW'Heavy DW'Light -> '╾'
  SolidDirs DW'Heavy DW'Light DW'None DW'None -> '╿'
  SolidDirs DW'None DW'Light DW'None DW'Light -> '┌'
  SolidDirs DW'None DW'Light DW'None DW'Heavy -> '┍'
  SolidDirs DW'None DW'Heavy DW'None DW'Light -> '┎'
  SolidDirs DW'None DW'Heavy DW'None DW'Heavy -> '┏'
  SolidDirs DW'None DW'Light DW'Light DW'None -> '┐'
  SolidDirs DW'None DW'Light DW'Heavy DW'None -> '┑'
  SolidDirs DW'None DW'Heavy DW'Light DW'None -> '┒'
  SolidDirs DW'None DW'Heavy DW'Heavy DW'None -> '┓'
  SolidDirs DW'Light DW'None DW'None DW'Light -> '└'
  SolidDirs DW'Light DW'None DW'None DW'Heavy -> '┕'
  SolidDirs DW'Heavy DW'None DW'None DW'Light -> '┖'
  SolidDirs DW'Heavy DW'None DW'None DW'Heavy -> '┗'
  SolidDirs DW'Light DW'None DW'Light DW'None -> '┘'
  SolidDirs DW'Light DW'None DW'Heavy DW'None -> '┙'
  SolidDirs DW'Heavy DW'None DW'Light DW'None -> '┚'
  SolidDirs DW'Heavy DW'None DW'Heavy DW'None -> '┛'
  SolidDirs DW'Light DW'Light DW'None DW'Light -> '├'
  SolidDirs DW'Light DW'Light DW'None DW'Heavy -> '┝'
  SolidDirs DW'Heavy DW'Light DW'None DW'Light -> '┞'
  SolidDirs DW'Light DW'Heavy DW'None DW'Light -> '┟'
  SolidDirs DW'Heavy DW'Heavy DW'None DW'Light -> '┠'
  SolidDirs DW'Heavy DW'Light DW'None DW'Heavy -> '┡'
  SolidDirs DW'Light DW'Heavy DW'None DW'Heavy -> '┢'
  SolidDirs DW'Heavy DW'Heavy DW'None DW'Heavy -> '┣'
  SolidDirs DW'Light DW'Light DW'Light DW'None -> '┤'
  SolidDirs DW'Light DW'Light DW'Heavy DW'None -> '┥'
  SolidDirs DW'Heavy DW'Light DW'Light DW'None -> '┦'
  SolidDirs DW'Light DW'Heavy DW'Light DW'None -> '┧'
  SolidDirs DW'Heavy DW'Heavy DW'Light DW'None -> '┨'
  SolidDirs DW'Heavy DW'Light DW'Heavy DW'None -> '┩'
  SolidDirs DW'Light DW'Heavy DW'Heavy DW'None -> '┪'
  SolidDirs DW'Heavy DW'Heavy DW'Heavy DW'None -> '┫'
  SolidDirs DW'None DW'Light DW'Light DW'Light -> '┬'
  SolidDirs DW'None DW'Light DW'Heavy DW'Light -> '┭'
  SolidDirs DW'None DW'Light DW'Light DW'Heavy -> '┮'
  SolidDirs DW'None DW'Light DW'Heavy DW'Heavy -> '┯'
  SolidDirs DW'None DW'Heavy DW'Light DW'Light -> '┰'
  SolidDirs DW'None DW'Heavy DW'Heavy DW'Light -> '┱'
  SolidDirs DW'None DW'Heavy DW'Light DW'Heavy -> '┲'
  SolidDirs DW'None DW'Heavy DW'Heavy DW'Heavy -> '┳'
  SolidDirs DW'Light DW'None DW'Light DW'Light -> '┴'
  SolidDirs DW'Light DW'None DW'Heavy DW'Light -> '┵'
  SolidDirs DW'Light DW'None DW'Light DW'Heavy -> '┶'
  SolidDirs DW'Light DW'None DW'Heavy DW'Heavy -> '┷'
  SolidDirs DW'Heavy DW'None DW'Light DW'Light -> '┸'
  SolidDirs DW'Heavy DW'None DW'Heavy DW'Light -> '┹'
  SolidDirs DW'Heavy DW'None DW'Light DW'Heavy -> '┺'
  SolidDirs DW'Heavy DW'None DW'Heavy DW'Heavy -> '┻'
  SolidDirs DW'Light DW'Light DW'Light DW'Light -> '┼'
  SolidDirs DW'Light DW'Light DW'Heavy DW'Light -> '┽'
  SolidDirs DW'Light DW'Light DW'Light DW'Heavy -> '┾'
  SolidDirs DW'Light DW'Light DW'Heavy DW'Heavy -> '┿'
  SolidDirs DW'Heavy DW'Light DW'Light DW'Light -> '╀'
  SolidDirs DW'Light DW'Heavy DW'Light DW'Light -> '╁'
  SolidDirs DW'Heavy DW'Heavy DW'Light DW'Light -> '╂'
  SolidDirs DW'Heavy DW'Light DW'Heavy DW'Light -> '╃'
  SolidDirs DW'Heavy DW'Light DW'Light DW'Heavy -> '╄'
  SolidDirs DW'Light DW'Heavy DW'Heavy DW'Light -> '╅'
  SolidDirs DW'Light DW'Heavy DW'Light DW'Heavy -> '╆'
  SolidDirs DW'Heavy DW'Light DW'Heavy DW'Heavy -> '╇'
  SolidDirs DW'Light DW'Heavy DW'Heavy DW'Heavy -> '╈'
  SolidDirs DW'Heavy DW'Heavy DW'Heavy DW'Light -> '╉'
  SolidDirs DW'Heavy DW'Heavy DW'Light DW'Heavy -> '╊'
  SolidDirs DW'Heavy DW'Heavy DW'Heavy DW'Heavy -> '╋'
  DoubleDirs D'Horiz False False True True -> '═'
  DoubleDirs D'Both False False True True -> '═'
  DoubleDirs D'Vert True True False False -> '║'
  DoubleDirs D'Both True True False False -> '║'
  DoubleDirs D'Horiz False True False True -> '╒'
  DoubleDirs D'Vert False True False True -> '╓'
  DoubleDirs D'Both False True False True -> '╔'
  DoubleDirs D'Horiz False True True False -> '╕'
  DoubleDirs D'Vert False True True False -> '╖'
  DoubleDirs D'Both False True True False -> '╗'
  DoubleDirs D'Horiz True False False True -> '╘'
  DoubleDirs D'Vert True False False True -> '╙'
  DoubleDirs D'Both True False False True -> '╚'
  DoubleDirs D'Horiz True False True False -> '╛'
  DoubleDirs D'Vert True False True False -> '╜'
  DoubleDirs D'Both True False True False -> '╝'
  DoubleDirs D'Horiz True True False True -> '╞'
  DoubleDirs D'Vert True True False True -> '╟'
  DoubleDirs D'Both True True False True -> '╠'
  DoubleDirs D'Horiz True True True False -> '╡'
  DoubleDirs D'Vert True True True False -> '╢'
  DoubleDirs D'Both True True True False -> '╣'
  DoubleDirs D'Horiz False True True True -> '╤'
  DoubleDirs D'Vert False True True True -> '╥'
  DoubleDirs D'Both False True True True -> '╦'
  DoubleDirs D'Horiz True False True True -> '╧'
  DoubleDirs D'Vert True False True True -> '╨'
  DoubleDirs D'Both True False True True -> '╩'
  DoubleDirs D'Horiz True True True True -> '╪'
  DoubleDirs D'Vert True True True True -> '╫'
  DoubleDirs D'Both True True True True -> '╬'
  _ -> ' '
