{-# LANGUAGE Strict #-}
{-# LANGUAGE NoFieldSelectors #-}

module Nice.Terminal.Render where

import Control.Monad.ST.Strict
import Data.Array.ST as Arr
import Data.Array.Unboxed
import Prelude hiding (State)

data Extent s = Extent
  { backing :: STUArray s (V2 Int) Char
  , size :: V2 Int
  }

modifyExtent :: V2 Int -> Extent s -> (Char -> Char) -> ST s (Extent s)
modifyExtent coord@(V2 x y) Extent {backing = backing0, size = size@(V2 w h)} modify = do
  cur <- readArray backing0 coord
  writeArray backing0 coord $! modify cur
  let sx = if x > w then 2 else 1
  let sy = if y > h then 2 else 1
  next <- newArray (0, V2 (sx * max x w) (sy * max y h)) ' '
  forM_ (range (0, size)) \i -> do
    orig <- readArray next i
    writeArray next i orig
  pure Extent {backing = next, size = max size coord}

-- --------------------------------------------------------------------------------

-- newtype ImageBuf = ImageBuf (forall s. Extent s -> ST s (Extent s))

-- instance Semigroup ImageBuf where
--   ImageBuf a <> ImageBuf b = ImageBuf (a >=> b)

-- instance Monoid ImageBuf where
--   mempty = ImageBuf pure

-- char :: V2 Int -> (Char -> Char) -> ImageBuf
-- char coord modifyChar =
--   fitImageBuf coord <> ImageBuf \buf -> do
--     cur <- readArray buf coord
--     writeArray buf coord $! modifyChar cur
--     pure buf

-- chars :: V2 Int -> [Char -> Char] -> ImageBuf
-- chars coord@(V2 x y) str =
--   fitImageBuf (coord + V2 (x + length str) y) <> ImageBuf \buf -> do
--     iforM_ str \i modifyChar -> do
--       cur <- readArray buf (V2 (x + i) y)
--       writeArray buf coord $! modifyChar cur
--     pure buf

-- str :: V2 Int -> String -> ImageBuf
-- str coord@(V2 x y) str =
--   fitImageBuf (coord + V2 (x + length str) y) <> ImageBuf \buf -> do
--     iforM_ str \i -> writeArray buf (V2 (x + i) y)
--     pure buf

-- --------------------------------------------------------------------------------

-- fitImageBuf :: V2 Int -> ImageBuf
-- fitImageBuf coord@(V2 x y) = ImageBuf \buf -> do
--   bounds@(_zeros, V2 w h) <- getBounds buf
--   if inRange bounds coord
--     then pure buf
--     else do
--       let sx = if x > w then 2 else 1
--       let sy = if y > h then 2 else 1
--       next <- newArray (0, V2 (sx * max x w) (sy * max y h)) ' '
--       forM_ (range bounds) \i -> do
--         orig <- readArray buf i
--         writeArray next i orig
--       pure next

-- --------------------------------------------------------------------------------

-- newtype Image = Image (UArray (V2 Int) Char)

-- renderImageBuf :: ImageBuf -> Image
-- renderImageBuf (ImageBuf writer) = Image $ runST do
--   buf0 <- newArray (V2 0 0, V2 0 0) ' '
--   final <- writer buf0
--   freeze final

-- putImageLn :: Image -> IO ()
-- putImageLn (Image buf) = do
--   let (_zeros, V2 w h) = bounds buf
--   forIntRange 0 (h + 1) \y -> do
--     str <- evaluatingStateT ([] :: [Char]) do
--       forIntRange 0 (w + 1) \x -> do
--         let ch = buf ! V2 x y
--         modify' (ch :)
--       gets (reverse . dropWhile (== ' '))
--     putStrLn str

-- imageSize :: Image -> V2 Int
-- imageSize (Image img) = snd (bounds img)
