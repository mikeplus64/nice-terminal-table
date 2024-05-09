module Nice.Terminal.Box.BorderStyles (
  BorderStyle,
  BoxBorder,
  asciiBorders,
  unicodeBorders,
  isBorderChar,
  readBorderChar,
  lookupBorderChar,
  runBorderStyle,
  -- * Borders
  up,
  down,
  left,
  right,
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
  -- * Styles
  heavy,
  heavyHoriz,
  heavyVert,
  dashed2,
  dashed3,
  double,
  doubleHoriz,
  doubleVert,
  rounded
) where

import Nice.Terminal.Box.Internal

readBorderChar :: BorderStyle -> Char -> BoxBorder
readBorderChar style ch = fromMaybe mempty (lookupBorderChar style ch)

asciiBorders :: BorderStyle
asciiBorders =
  $( mkBorderStyle
      [ (horizontal, '-')
      , (vertical, '|')
      , (cornerTL, '+')
      , (cornerTR, '+')
      , (cornerBR, '+')
      , (cornerBL, '+')
      , (intersectFull, '+')
      , (intersectL, '+')
      , (intersectR, '+')
      , (intersectT, '+')
      , (intersectB, '+')
      ]
  )

unicodeBorders :: BorderStyle
unicodeBorders =
  $( mkBorderStyle
      [ (up, '╵')
      , (down, '╷')
      , (left, '╴')
      , (right, '╶')
      , (horizontal, '─')
      , (vertical, '│')
      , (cornerTL, '┌')
      , (cornerTR, '┐')
      , (cornerBR, '┘')
      , (cornerBL, '└')
      , (intersectFull, '┼')
      , (intersectL, '├')
      , (intersectR, '┤')
      , (intersectT, '┬')
      , (intersectB, '┴')
      , (heavy <> up, '╹')
      , (heavy <> down, '╻')
      , (heavy <> left, '╸')
      , (heavy <> right, '╺')
      , (heavy <> cornerTL, '┏')
      , (heavy <> cornerTR, '┓')
      , (heavy <> cornerBR, '┛')
      , (heavy <> cornerBL, '┗')
      , (heavy <> intersectFull, '╋')
      , (heavy <> intersectL, '┣')
      , (heavy <> intersectR, '┫')
      , (heavy <> intersectT, '┳')
      , (heavy <> intersectB, '┻')
      , (heavy <> horizontal, '━')
      , (heavy <> vertical, '┃')
      , (heavyHoriz <> cornerTL, '┍')
      , (heavyHoriz <> cornerTR, '┑')
      , (heavyHoriz <> cornerBR, '┙')
      , (heavyHoriz <> cornerBL, '┕')
      , (heavyHoriz <> intersectFull, '┿')
      , (heavyHoriz <> intersectL, '┝')
      , (heavyHoriz <> intersectR, '┥')
      , (heavyHoriz <> intersectT, '┯')
      , (heavyHoriz <> intersectB, '┷')
      , (heavyHoriz <> horizontal, '━')
      , (heavyVert <> cornerTL, '┎')
      , (heavyVert <> cornerTR, '┒')
      , (heavyVert <> cornerBR, '┚')
      , (heavyVert <> cornerBL, '┕')
      , (heavyVert <> intersectFull, '╂')
      , (heavyVert <> intersectL, '┠')
      , (heavyVert <> intersectR, '┨')
      , (heavyVert <> intersectT, '┰')
      , (heavyVert <> intersectB, '┸')
      , (heavyVert <> vertical, '┃')
      , (rounded <> cornerTL, '╭')
      , (rounded <> cornerTR, '╮')
      , (rounded <> cornerBR, '╯')
      , (rounded <> cornerBL, '╰')
      , (dashed2 <> horizontal, '╌')
      , (dashed2 <> vertical, '╎')
      , (dashed3 <> horizontal, '┄')
      , (dashed3 <> vertical, '┆')
      , (dashed4 <> horizontal, '┈')
      , (dashed4 <> vertical, '┊')
      , (double <> horizontal, '═')
      , (double <> vertical, '║')
      , (double <> intersectL, '╠')
      , (double <> intersectR, '╣')
      , (double <> intersectT, '╦')
      , (double <> intersectB, '╩')
      , (double <> cornerTL, '╔')
      , (double <> cornerTR, '╗')
      , (double <> cornerBR, '╝')
      , (double <> cornerBL, '╚')
      , (doubleHoriz <> horizontal, '═')
      , (doubleHoriz <> vertical, '│')
      , (doubleHoriz <> intersectL, '╞')
      , (doubleHoriz <> intersectR, '╡')
      , (doubleHoriz <> intersectT, '╤')
      , (doubleHoriz <> intersectB, '╧')
      , (doubleHoriz <> cornerTL, '╒')
      , (doubleHoriz <> cornerTR, '╕')
      , (doubleHoriz <> cornerBR, '╛')
      , (doubleHoriz <> cornerBL, '╘')
      , (doubleVert <> horizontal, '─')
      , (doubleVert <> vertical, '║')
      , (doubleVert <> intersectL, '╟')
      , (doubleVert <> intersectR, '╢')
      , (doubleVert <> intersectT, '╥')
      , (doubleVert <> intersectB, '╨')
      , (doubleVert <> cornerTL, '╓')
      , (doubleVert <> cornerTR, '╖')
      , (doubleVert <> cornerBR, '╜')
      , (doubleVert <> cornerBL, '╙')
      ]
  )
