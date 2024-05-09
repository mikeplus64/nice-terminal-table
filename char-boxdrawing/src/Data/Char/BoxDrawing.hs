{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Char.BoxDrawing (
  -- * Box border type
  Drawing,
  render,
  lookup,
  read,
  overlay,

  -- ** Box drawing styles
  BoxDrawingStyle,
  ascii,
  unicode,

  -- * Drawings

  -- ** Base primitives
  -- $primitives
  up,
  down,
  left,
  right,

  -- ** Combinations
  -- $combinations
  horizontal,
  vertical,
  cornerTL,
  cornerTR,
  cornerBR,
  cornerBL,
  intersectFull,
  intersectL,
  intersectR,
  intersectT,
  intersectB,

  -- ** Box drawing modifiers
  -- $modifiers
  heavy,
  heavyHoriz,
  heavyVert,
  dashed2,
  dashed3,
  double,
  doubleHoriz,
  doubleVert,
  rounded,
) where

import Data.Bits
import Data.Coerce
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Text.Printf (printf)
import Prelude hiding (lookup, read)

--------------------------------------------------------------------------------
--

-- $primitives
--
-- Use the 'Semigroup' instance to combine box 'Drawing'.

up, left, right, down, cornerTL, cornerTR, cornerBR, cornerBL, horizontal, vertical, intersectL, intersectR, intersectT, intersectB, intersectFull :: Drawing
up = bb BUp
left = bb BLeft
right = bb BRight
down = bb BDown

-- $combinations
--
-- These are pre-made combinations of 'up', 'down', 'left', and 'right'.

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

-- $modifiers
--
-- Combine these using the 'Semigroup' instance with another 'Drawing' to set
-- its style, such as heavy, dashed, etc. - only relevant for the style
-- 'unicode' and has no effect for the style 'ascii'.

double, doubleHoriz, doubleVert, heavy, dashed2, dashed3, dashed4, rounded, heavyHoriz, heavyVert :: Drawing
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

bb :: Drawing1 -> Drawing
bb = boxBorderBits

--------------------------------------------------------------------------------

-- | A border style such as ASCII or unicode. Describes how to turn a
-- 'Drawing' into an actual character for rendering, or to lookup a character
-- from a buffer into a 'Drawing' for combining different border characters
data BoxDrawingStyle = BoxDrawingStyle
  { render :: Drawing -> Char
  -- ^ Render a 'Drawing' given the style
  , lookup :: Char -> Maybe Drawing
  -- ^ Lookup a character given the style
  }

--------------------------------------------------------------------------------
-- Generated code by using TH and then copy-pasting it back with -ddump-splices
--
-- Hopefully, there are no bugs in this ever :)
--

-- | A basic box-drawing style that uses @|@, @+@, and @-@
ascii :: BoxDrawingStyle
ascii =
  ( \(run :: Word8 -> Char)
     (runBack :: Char -> Maybe Word8) ->
        BoxDrawingStyle (coerce run) (coerce runBack)
  )
    ( \case
        3 -> '|'
        5 -> '+'
        6 -> '+'
        7 -> '+'
        9 -> '+'
        10 -> '+'
        11 -> '+'
        12 -> '-'
        13 -> '+'
        14 -> '+'
        15 -> '+'
        19 -> '|'
        21 -> '+'
        22 -> '+'
        23 -> '+'
        25 -> '+'
        26 -> '+'
        27 -> '+'
        28 -> '-'
        29 -> '+'
        30 -> '+'
        31 -> '+'
        35 -> '|'
        37 -> '+'
        38 -> '+'
        39 -> '+'
        41 -> '+'
        42 -> '+'
        43 -> '+'
        44 -> '-'
        45 -> '+'
        46 -> '+'
        47 -> '+'
        51 -> '|'
        53 -> '+'
        54 -> '+'
        55 -> '+'
        57 -> '+'
        58 -> '+'
        59 -> '+'
        60 -> '-'
        61 -> '+'
        62 -> '+'
        63 -> '+'
        67 -> '|'
        69 -> '+'
        70 -> '+'
        71 -> '+'
        73 -> '+'
        74 -> '+'
        75 -> '+'
        76 -> '-'
        77 -> '+'
        78 -> '+'
        79 -> '+'
        83 -> '|'
        85 -> '+'
        86 -> '+'
        87 -> '+'
        89 -> '+'
        90 -> '+'
        91 -> '+'
        92 -> '-'
        93 -> '+'
        94 -> '+'
        95 -> '+'
        99 -> '|'
        101 -> '+'
        102 -> '+'
        103 -> '+'
        105 -> '+'
        106 -> '+'
        107 -> '+'
        108 -> '-'
        109 -> '+'
        110 -> '+'
        111 -> '+'
        115 -> '|'
        117 -> '+'
        118 -> '+'
        119 -> '+'
        121 -> '+'
        122 -> '+'
        123 -> '+'
        124 -> '-'
        125 -> '+'
        126 -> '+'
        127 -> '+'
        131 -> '|'
        133 -> '+'
        134 -> '+'
        135 -> '+'
        137 -> '+'
        138 -> '+'
        139 -> '+'
        140 -> '-'
        141 -> '+'
        142 -> '+'
        143 -> '+'
        147 -> '|'
        149 -> '+'
        150 -> '+'
        151 -> '+'
        153 -> '+'
        154 -> '+'
        155 -> '+'
        156 -> '-'
        157 -> '+'
        158 -> '+'
        159 -> '+'
        163 -> '|'
        165 -> '+'
        166 -> '+'
        167 -> '+'
        169 -> '+'
        170 -> '+'
        171 -> '+'
        172 -> '-'
        173 -> '+'
        174 -> '+'
        175 -> '+'
        179 -> '|'
        181 -> '+'
        182 -> '+'
        183 -> '+'
        185 -> '+'
        186 -> '+'
        187 -> '+'
        188 -> '-'
        189 -> '+'
        190 -> '+'
        191 -> '+'
        195 -> '|'
        197 -> '+'
        198 -> '+'
        199 -> '+'
        201 -> '+'
        202 -> '+'
        203 -> '+'
        204 -> '-'
        205 -> '+'
        206 -> '+'
        207 -> '+'
        211 -> '|'
        213 -> '+'
        214 -> '+'
        215 -> '+'
        217 -> '+'
        218 -> '+'
        219 -> '+'
        220 -> '-'
        221 -> '+'
        222 -> '+'
        223 -> '+'
        227 -> '|'
        229 -> '+'
        230 -> '+'
        231 -> '+'
        233 -> '+'
        234 -> '+'
        235 -> '+'
        236 -> '-'
        237 -> '+'
        238 -> '+'
        239 -> '+'
        243 -> '|'
        245 -> '+'
        246 -> '+'
        247 -> '+'
        249 -> '+'
        250 -> '+'
        251 -> '+'
        252 -> '-'
        253 -> '+'
        254 -> '+'
        255 -> '+'
        _ -> ' '
    )
    ( \case
        '+' -> Just 255
        '-' -> Just 252
        '|' -> Just 243
        _ -> Nothing
    )

-- | An advanced box-drawing style that supports all the drawing primitives
-- provided in this module
unicode :: BoxDrawingStyle
unicode =
  ( \(run :: Word8 -> Char)
     (runBack :: Char -> Maybe Word8) ->
        BoxDrawingStyle (coerce run) (coerce runBack)
  )
    ( \case
        1 -> '\9589'
        2 -> '\9591'
        3 -> '\9474'
        4 -> '\9588'
        5 -> '\9496'
        6 -> '\9488'
        7 -> '\9508'
        8 -> '\9590'
        9 -> '\9492'
        10 -> '\9484'
        11 -> '\9500'
        12 -> '\9472'
        13 -> '\9524'
        14 -> '\9516'
        15 -> '\9532'
        17 -> '\9589'
        18 -> '\9591'
        19 -> '\9474'
        20 -> '\9588'
        21 -> '\9583'
        22 -> '\9582'
        23 -> '\9508'
        24 -> '\9590'
        25 -> '\9584'
        26 -> '\9581'
        27 -> '\9500'
        28 -> '\9472'
        29 -> '\9524'
        30 -> '\9516'
        31 -> '\9532'
        33 -> '\9589'
        34 -> '\9591'
        35 -> '\9550'
        36 -> '\9588'
        37 -> '\9496'
        38 -> '\9488'
        39 -> '\9508'
        40 -> '\9590'
        41 -> '\9492'
        42 -> '\9484'
        43 -> '\9500'
        44 -> '\9548'
        45 -> '\9524'
        46 -> '\9516'
        47 -> '\9532'
        49 -> '\9589'
        50 -> '\9591'
        51 -> '\9474'
        52 -> '\9588'
        53 -> '\9496'
        54 -> '\9488'
        55 -> '\9508'
        56 -> '\9590'
        57 -> '\9492'
        58 -> '\9484'
        59 -> '\9500'
        60 -> '\9472'
        61 -> '\9524'
        62 -> '\9516'
        63 -> '\9532'
        65 -> '\9589'
        66 -> '\9591'
        67 -> '\9478'
        68 -> '\9588'
        69 -> '\9496'
        70 -> '\9488'
        71 -> '\9508'
        72 -> '\9590'
        73 -> '\9492'
        74 -> '\9484'
        75 -> '\9500'
        76 -> '\9476'
        77 -> '\9524'
        78 -> '\9516'
        79 -> '\9532'
        81 -> '\9589'
        82 -> '\9591'
        83 -> '\9474'
        84 -> '\9588'
        85 -> '\9496'
        86 -> '\9488'
        87 -> '\9508'
        88 -> '\9590'
        89 -> '\9492'
        90 -> '\9484'
        91 -> '\9500'
        92 -> '\9472'
        93 -> '\9524'
        94 -> '\9516'
        95 -> '\9532'
        97 -> '\9589'
        98 -> '\9591'
        99 -> '\9482'
        100 -> '\9588'
        101 -> '\9496'
        102 -> '\9488'
        103 -> '\9508'
        104 -> '\9590'
        105 -> '\9492'
        106 -> '\9484'
        107 -> '\9500'
        108 -> '\9480'
        109 -> '\9524'
        110 -> '\9516'
        111 -> '\9532'
        113 -> '\9589'
        114 -> '\9591'
        115 -> '\9474'
        116 -> '\9588'
        117 -> '\9496'
        118 -> '\9488'
        119 -> '\9508'
        120 -> '\9590'
        121 -> '\9492'
        122 -> '\9484'
        123 -> '\9500'
        124 -> '\9472'
        125 -> '\9524'
        126 -> '\9516'
        127 -> '\9532'
        129 -> '\9589'
        130 -> '\9591'
        131 -> '\9474'
        132 -> '\9588'
        133 -> '\9497'
        134 -> '\9489'
        135 -> '\9509'
        136 -> '\9590'
        137 -> '\9493'
        138 -> '\9485'
        139 -> '\9501'
        140 -> '\9473'
        141 -> '\9527'
        142 -> '\9519'
        143 -> '\9535'
        145 -> '\9589'
        146 -> '\9591'
        147 -> '\9475'
        148 -> '\9588'
        149 -> '\9498'
        150 -> '\9490'
        151 -> '\9512'
        152 -> '\9590'
        153 -> '\9493'
        154 -> '\9486'
        155 -> '\9504'
        156 -> '\9472'
        157 -> '\9528'
        158 -> '\9520'
        159 -> '\9538'
        161 -> '\9593'
        162 -> '\9595'
        163 -> '\9475'
        164 -> '\9592'
        165 -> '\9499'
        166 -> '\9491'
        167 -> '\9515'
        168 -> '\9594'
        169 -> '\9495'
        170 -> '\9487'
        171 -> '\9507'
        172 -> '\9473'
        173 -> '\9531'
        174 -> '\9523'
        175 -> '\9547'
        177 -> '\9589'
        178 -> '\9591'
        179 -> '\9474'
        180 -> '\9588'
        181 -> '\9563'
        182 -> '\9557'
        183 -> '\9569'
        184 -> '\9590'
        185 -> '\9560'
        186 -> '\9554'
        187 -> '\9566'
        188 -> '\9552'
        189 -> '\9575'
        190 -> '\9572'
        191 -> '\9532'
        193 -> '\9589'
        194 -> '\9591'
        195 -> '\9553'
        196 -> '\9588'
        197 -> '\9564'
        198 -> '\9558'
        199 -> '\9570'
        200 -> '\9590'
        201 -> '\9561'
        202 -> '\9555'
        203 -> '\9567'
        204 -> '\9472'
        205 -> '\9576'
        206 -> '\9573'
        207 -> '\9532'
        209 -> '\9589'
        210 -> '\9591'
        211 -> '\9553'
        212 -> '\9588'
        213 -> '\9565'
        214 -> '\9559'
        215 -> '\9571'
        216 -> '\9590'
        217 -> '\9562'
        218 -> '\9556'
        219 -> '\9568'
        220 -> '\9552'
        221 -> '\9577'
        222 -> '\9574'
        223 -> '\9532'
        225 -> '\9589'
        226 -> '\9591'
        227 -> '\9474'
        228 -> '\9588'
        229 -> '\9496'
        230 -> '\9488'
        231 -> '\9508'
        232 -> '\9590'
        233 -> '\9492'
        234 -> '\9484'
        235 -> '\9500'
        236 -> '\9472'
        237 -> '\9524'
        238 -> '\9516'
        239 -> '\9532'
        241 -> '\9589'
        242 -> '\9591'
        243 -> '\9474'
        244 -> '\9588'
        245 -> '\9496'
        246 -> '\9488'
        247 -> '\9508'
        248 -> '\9590'
        249 -> '\9492'
        250 -> '\9484'
        251 -> '\9500'
        252 -> '\9472'
        253 -> '\9524'
        254 -> '\9516'
        255 -> '\9532'
        _ -> ' '
    )
    ( \case
        '\9472' -> Just 252
        '\9473' -> Just 172
        '\9474' -> Just 243
        '\9475' -> Just 163
        '\9476' -> Just 76
        '\9478' -> Just 67
        '\9480' -> Just 108
        '\9482' -> Just 99
        '\9484' -> Just 250
        '\9485' -> Just 138
        '\9486' -> Just 154
        '\9487' -> Just 170
        '\9488' -> Just 246
        '\9489' -> Just 134
        '\9490' -> Just 150
        '\9491' -> Just 166
        '\9492' -> Just 249
        '\9493' -> Just 153
        '\9495' -> Just 169
        '\9496' -> Just 245
        '\9497' -> Just 133
        '\9498' -> Just 149
        '\9499' -> Just 165
        '\9500' -> Just 251
        '\9501' -> Just 139
        '\9504' -> Just 155
        '\9507' -> Just 171
        '\9508' -> Just 247
        '\9509' -> Just 135
        '\9512' -> Just 151
        '\9515' -> Just 167
        '\9516' -> Just 254
        '\9519' -> Just 142
        '\9520' -> Just 158
        '\9523' -> Just 174
        '\9524' -> Just 253
        '\9527' -> Just 141
        '\9528' -> Just 157
        '\9531' -> Just 173
        '\9532' -> Just 255
        '\9535' -> Just 143
        '\9538' -> Just 159
        '\9547' -> Just 175
        '\9548' -> Just 44
        '\9550' -> Just 35
        '\9552' -> Just 220
        '\9553' -> Just 211
        '\9554' -> Just 186
        '\9555' -> Just 202
        '\9556' -> Just 218
        '\9557' -> Just 182
        '\9558' -> Just 198
        '\9559' -> Just 214
        '\9560' -> Just 185
        '\9561' -> Just 201
        '\9562' -> Just 217
        '\9563' -> Just 181
        '\9564' -> Just 197
        '\9565' -> Just 213
        '\9566' -> Just 187
        '\9567' -> Just 203
        '\9568' -> Just 219
        '\9569' -> Just 183
        '\9570' -> Just 199
        '\9571' -> Just 215
        '\9572' -> Just 190
        '\9573' -> Just 206
        '\9574' -> Just 222
        '\9575' -> Just 189
        '\9576' -> Just 205
        '\9577' -> Just 221
        '\9581' -> Just 26
        '\9582' -> Just 22
        '\9583' -> Just 21
        '\9584' -> Just 25
        '\9588' -> Just 244
        '\9589' -> Just 241
        '\9590' -> Just 248
        '\9591' -> Just 242
        '\9592' -> Just 164
        '\9593' -> Just 161
        '\9594' -> Just 168
        '\9595' -> Just 162
        _ -> Nothing
    )

--------------------------------------------------------------------------------

-- | A box border which can be rendered to a terminal given a 'BoxDrawingStyle'
--
-- Use the 'Monoid' and 'Semigroup' instances to combine box borders.
--
-- The internal representation is just a 'Word8'; the least-significant four
-- bits specify the base directions of the box drawing (up, down, left, or
-- right), and the most-significant four bits specify the style (see 'rounded',
-- 'dashed2', etc) which has its own special logic for how styles can be
-- combined.
newtype Drawing = Drawing Word8
  deriving newtype (Eq, Ord)

instance Semigroup Drawing where
  (<>) a@(Drawing ab) b@(Drawing bb) =
    Drawing ((ab .|. bb) .&. 0x0f) `setStyle1` (getStyle1 a <> getStyle1 b)

instance Monoid Drawing where
  mempty = Drawing 0

-- | A single box border component
data Drawing1
  = BUp
  | BDown
  | BLeft
  | BRight
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

newtype StyleBits = StyleBits Word8

styleBits :: Drawing -> StyleBits
styleBits (Drawing fs) = StyleBits (fs `shiftR` 4)

setStyleBits :: Drawing -> StyleBits -> Drawing
setStyleBits (Drawing fs) (StyleBits s) = Drawing ((fs .&. 0x0f) .|. (s `shiftL` 4))

setStyle1 :: Drawing -> Style1 -> Drawing
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

style1 :: Style1 -> Drawing
style1 s = setStyleBits mempty (fromStyle1 s)

fromStyle1 :: Style1 -> StyleBits
fromStyle1 = StyleBits . toEnum . fromEnum

dropStyleBits :: Drawing -> Drawing
dropStyleBits (Drawing w) = Drawing (w .&. 0x0f)

getStyle1 :: Drawing -> Style1
getStyle1 (Drawing w)
  | i >= 0 && i <= fromEnum (maxBound @Style1) = toEnum i
  | otherwise = mempty
  where
    !i = fromIntegral (w `shiftR` 4)

getBorderBitField :: Drawing -> Word8
getBorderBitField (Drawing fs) = fs

bitsToDrawing :: Drawing -> [Drawing1]
bitsToDrawing (Drawing fs) = concat do
  !b1 <- [minBound .. maxBound @Drawing1]
  pure [b1 | testBit fs (fromEnum b1)]

boxBorderBits :: Drawing1 -> Drawing
boxBorderBits = Drawing . bit . fromEnum

-- | Given a box drawing style and a character, determine what 'Drawing' the
-- character is. On error, returns an empty 'Drawing'
read :: BoxDrawingStyle -> Char -> Drawing
read style ch = fromMaybe mempty (lookup style ch)

-- | Get the character that best represents visually overlaying the given box
-- drawing and a character.
overlay :: BoxDrawingStyle -> Drawing -> Char -> Char
overlay style next cur = render style (read style cur <> next)
