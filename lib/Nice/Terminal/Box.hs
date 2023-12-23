module Nice.Terminal.Box where

import Linear (V2 (..))
import Nice.Terminal.Box.Intersection
import Nice.Terminal.Render

--------------------------------------------------------------------------------
-- Reproduced from the "brick" terminal UI library

drawBox :: V2 Int -> V2 Int -> Render ()
drawBox (V2 i0 j0) (V2 i1 j1) = do
  let border p s = do
        modifyBuf p (\ex -> combineBorders style ex (base <> s))
  forIntRange (i0 + 1) i1 \i -> do
    border (V2 i j0) horizontal
    border (V2 i j1) horizontal
  forIntRange (j0 + 1) j1 \j -> do
    border (V2 i0 j) vertical
    border (V2 i1 j) vertical
  border (V2 i0 j0) cornerTL
  border (V2 i0 j1) cornerBL
  border (V2 i1 j1) cornerBR
  border (V2 i1 j0) cornerTR

test = runRender do
  drawBox unicodeBorders heavy (V2 0 0) (V2 10 10)
  drawBox unicodeBorders rounded (V2 3 3) (V2 10 10)
  drawBox unicodeBorders doubleHoriz (V2 2 2) (V2 6 8)
  popBuf
  drawBox unicodeBorders heavy (V2 11 11) (V2 14 14)
  popBuf
