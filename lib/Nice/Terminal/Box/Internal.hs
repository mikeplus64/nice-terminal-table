{-# LANGUAGE MagicHash #-}

module Nice.Terminal.Box.Internal where

import Data.Bits
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH qualified as TH
import Text.Printf (printf)
import Text.Show (Show (show))
import Prelude hiding (show)

-- | A single box border component
data BoxBorder1
  = BUp
  | BDown
  | BLeft
  | BRight
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

boxBorderBit :: BoxBorder1 -> Int
boxBorderBit = \case
  BUp -> 0
  BDown -> 1
  BLeft -> 2
  BRight -> 3

-- | A box border which can be rendered to a terminal given a 'BorderStyle'
--
-- Use the 'Monoid' and 'Semigroup' instances to combine box borders.
newtype BoxBorder = BoxBorder Word8
  deriving newtype (Eq, Ord, Enum, Bounded, NFData)

newtype StyleBits = StyleBits Word8

styleBits :: BoxBorder -> StyleBits
styleBits (BoxBorder fs) = StyleBits (fs `shiftR` 4)

setStyleBits :: BoxBorder -> StyleBits -> BoxBorder
setStyleBits (BoxBorder fs) (StyleBits s) = BoxBorder ((fs .&. 0x0f) .|. (s `shiftL` 4))

setStyle1 :: BoxBorder -> Style1 -> BoxBorder
setStyle1 b s = setStyleBits b (fromStyle1 s)

data Style1
  = Light
  | Rounded
  | Dashed2
  | Dashed2Heavy
  | Dashed3
  | Dashed3Heavy
  | Dashed4
  | Dashed4Heavy
  | HeavyHoriz
  | HeavyVert
  | Heavy
  | DoubleHoriz
  | DoubleVert
  | Double
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Monoid Style1 where
  mempty = Light

instance Semigroup Style1 where
  (<>) DoubleHoriz DoubleVert = Double
  (<>) DoubleVert DoubleHoriz = Double
  (<>) HeavyHoriz HeavyVert = Heavy
  (<>) HeavyVert HeavyHoriz = Heavy
  (<>) Heavy Dashed2 = Dashed2Heavy
  (<>) Dashed2 Heavy = Dashed2Heavy
  (<>) Heavy Dashed3 = Dashed3Heavy
  (<>) Dashed3 Heavy = Dashed3Heavy
  (<>) Heavy Dashed4 = Dashed4Heavy
  (<>) Dashed4 Heavy = Dashed4Heavy
  (<>) a b = max a b

style1 :: Style1 -> BoxBorder
style1 s = setStyleBits mempty (fromStyle1 s)

fromStyle1 :: Style1 -> StyleBits
fromStyle1 = StyleBits . toEnum . fromEnum

dropStyleBits :: BoxBorder -> BoxBorder
dropStyleBits (BoxBorder w) = BoxBorder (w .&. 0x0f)

getStyle1 :: BoxBorder -> Style1
getStyle1 (BoxBorder w)
  | i >= 0 && i <= fromEnum (maxBound @Style1) = toEnum i
  | otherwise = mempty
  where
    !i = fromIntegral (w `shiftR` 4)

getBorderBitField :: BoxBorder -> Word8
getBorderBitField (BoxBorder fs) = fs

instance Show BoxBorder where
  show b@(BoxBorder bits) = printf "(BoxBorder [0b%08b] %s %s)" bits (show (bitsToBoxBorder b)) (show (getStyle1 b))

instance Semigroup BoxBorder where
  (<>) a@(BoxBorder ab) b@(BoxBorder bb) =
    BoxBorder ((ab .|. bb) .&. 0x0f) `setStyle1` (getStyle1 a <> getStyle1 b)

instance Monoid BoxBorder where
  mempty = BoxBorder 0

bitsToBoxBorder :: BoxBorder -> [BoxBorder1]
bitsToBoxBorder (BoxBorder fs) = concat do
  !b1 <- [minBound .. maxBound @BoxBorder1]
  pure [b1 | testBit fs (boxBorderBit b1)]

boxBorderBits :: BoxBorder1 -> BoxBorder
boxBorderBits = BoxBorder . bit . boxBorderBit

--------------------------------------------------------------------------------

bb :: BoxBorder1 -> BoxBorder
bb = boxBorderBits

-- Combinations
-- ============

up, left, right, down, cornerTL, cornerTR, cornerBR, cornerBL, horizontal, vertical, intersectL, intersectR, intersectT, intersectB, intersectFull :: BoxBorder
up = bb BUp
left = bb BLeft
right = bb BRight
down = bb BDown
cornerTL = bb BDown <> bb BRight
cornerTR = bb BDown <> bb BLeft
cornerBR = bb BUp <> bb BLeft
cornerBL = bb BUp <> bb BRight
horizontal = bb BLeft <> bb BRight
vertical = bb BUp <> bb BDown
intersectL = vertical <> right
intersectR = vertical <> left
intersectT = horizontal <> down
intersectB = horizontal <> up
intersectFull = horizontal <> vertical

double, doubleHoriz, doubleVert, heavy, dashed2, dashed3, dashed4, rounded, heavyHoriz, heavyVert :: BoxBorder
double = style1 Double
doubleHoriz = style1 DoubleHoriz
doubleVert = style1 DoubleVert
heavy = style1 Heavy
heavyHoriz = style1 HeavyHoriz
heavyVert = style1 HeavyVert
dashed2 = style1 Dashed2
dashed3 = style1 Dashed3
dashed4 = style1 Dashed4
rounded = style1 Rounded

-- | A border style such as ASCII or unicode. Describes how to turn a
-- 'BoxBorder' into an actual character for rendering, or to lookup a character
-- from a buffer into a 'BoxBorder' for combining different border characters
--
-- Construct a 'BorderStyle' with 'mkBorderStyle'
data BorderStyle = BorderStyle
  { runBorderStyle :: BoxBorder -> Char
  , lookupBorderChar :: Char -> Maybe BoxBorder
  , isBorderChar :: Char -> Bool
  }

-- | Construct an optimised 'BorderStyle' from a list of associations to
-- characters
mkBorderStyle :: [(BoxBorder, Char)] -> ExpQ
mkBorderStyle = optimiseBorderStyle . borderStyleFromAssocs

-- | "Optimise" a 'BorderStyle' by turning it into exhaustive pattern matches on
-- inputs
optimiseBorderStyle :: BorderStyle -> ExpQ
optimiseBorderStyle BorderStyle {runBorderStyle} =
  [|
    ( \(run :: Word8 -> Char) (runBack :: Char -> Maybe Word8) (check :: Char -> Bool) ->
        BorderStyle
          (coerce run)
          (coerce runBack)
          check
    )
    |]
    `TH.appE` borderStyleFn
    `TH.appE` borderStyleBackFn
    `TH.appE` isBorderCharFn
  where
    blanks = filter ((== ' ') . snd) allMatches
    matches = filter ((/= ' ') . snd) allMatches
    allMatches =
      [ (b, ch) | b <- [minBound .. maxBound], let ch = runBorderStyle (BoxBorder b)
      ]
    borderStyleFn =
      TH.lamCaseE $
        [ TH.match (TH.litP (TH.integerL (toInteger b))) (TH.normalB (TH.litE (TH.charL ch))) []
        | (b, ch) <- matches
        ]
          ++ [TH.match TH.wildP (TH.normalB (TH.litE (TH.charL ' '))) [] | not (null blanks)]
    borderStyleBackFn =
      TH.lamCaseE $
        [ TH.match
          (TH.litP (TH.charL ch))
          (TH.normalB ([|Just|] `TH.appE` TH.litE (TH.integerL (toInteger b))))
          []
        | (ch, b) <- Map.toList (Map.fromList (map (\(a, b) -> (b, a)) matches))
        ]
          ++ [ TH.match
              TH.wildP
              (TH.normalB [|Nothing|])
              []
             | not (null blanks)
             ]
    isBorderCharFn =
      TH.lamCaseE $
        [ TH.match (TH.litP (TH.charL ch)) (TH.normalB [|True|]) []
        | ch <- Set.toList (Set.fromList (map snd matches))
        ]
          ++ [TH.match TH.wildP (TH.normalB [|False|]) []]

-- | Create a 'BorderStyle' from a list of associations
borderStyleFromAssocs :: [(BoxBorder, Char)] -> BorderStyle
borderStyleFromAssocs list =
  let
    assocs :: IntMap Char
    !assocs = IntMap.fromList (map (first fromEnum) list)

    assocsBack :: IntMap BoxBorder
    !assocsBack = IntMap.fromList (map (\(a, b) -> (fromEnum b, a)) list)

    chars :: IntSet
    !chars = IntSet.fromList (map (fromEnum . snd) list)
   in
    rnf (assocs, assocsBack, chars) `seq`
      BorderStyle
        { runBorderStyle = \b ->
            if
                | b == mempty -> ' '
                | Just ch <- assocs ^. at (fromEnum b), ch /= ' ' -> ch
                | Just ch <- assocs ^. at (fromEnum (dropStyleBits b)) -> ch
                | otherwise -> ' '
        , lookupBorderChar = \c -> assocsBack ^. at (fromEnum c)
        , isBorderChar = \c -> fromEnum c `IntSet.member` chars
        }
