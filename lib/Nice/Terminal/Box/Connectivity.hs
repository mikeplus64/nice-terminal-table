module Nice.Terminal.Box.Connectivity where

data CentreConnectivity
  = Light
  | Heavy
  | Double
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data Eighth
  = E1
  | E2
  | E3
  | E4
  | E5
  | E6
  | E7
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data Axis = Horizontal | Vertical
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data FullBlockType
  = LightShade
  | MediumShade
  | InverseMediumShade
  | DarkShade
  | Checkerboard
  | InverseCheckerboard
  deriving stock (Show, Eq, Ord)

data Connectable (axis :: Axis)
  = Lower !Eighth
  | Middle !CentreConnectivity
  | Higher !Eighth
  | FullBlock !FullBlockType
  | HalfShadeLower
  | HalfShadeHigher
  | Blank
  deriving stock (Show, Eq, Ord)

class KnownAxis (axis :: Axis) where knownAxis :: Axis
instance KnownAxis 'Horizontal where knownAxis = Horizontal
instance KnownAxis 'Vertical where knownAxis = Vertical

data Connectivity = Connectivity
  { left, right :: !(Set (Connectable Horizontal))
  , top, bottom :: !(Set (Connectable Vertical))
  }
  deriving stock (Show, Eq, Ord)
