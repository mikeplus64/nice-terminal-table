{-# LANUAGE MagicHash #-}
{-# LANGUAGE NoFieldSelectors #-}

module Nice.Terminal.Render where

import Data.Array.IO as Arr
import Data.Array.Unboxed
import Data.Ix
import Prelude hiding (State)

type Buf = IOUArray (V2 Int) Char

newtype Env = Env
  { printBuf :: Buf -> Render ()
  }

data State = State
  { offsetY :: !Int
  , buf :: !Buf
  , maxCoord :: !(V2 Int)
  }

newtype Render a = Render
  { unRender :: Env -> State -> IO (a, State)
  }
  deriving (Functor, Applicative, Monad, MonadState State, MonadReader Env) via (ReaderT Env (StateT State IO))

instance MonadIO Render where
  liftIO m = Render \_ s -> (,s) <$> m

print2dArray :: IOUArray (V2 Int) Char -> Render ()
print2dArray arr = do
  State {maxCoord = V2 w h, offsetY} <- get
  liftIO $ forIntRange 0 ((h + 1) - offsetY) \y -> do
    evaluatingStateT ([] :: [Char]) do
      forIntRange 0 (w + 1) \x -> do
        ch <- liftIO (readArray arr (V2 x y))
        modify' (ch :)
      str <- gets (reverse . dropWhile (== ' '))
      putStrLn str

runRender :: Render a -> IO a
runRender (Render f) = do
  buf <- newArray (0, V2 60 60) ' '
  (a, _) <- f (Env print2dArray) State {offsetY = 0, buf, maxCoord = -1}
  pure a

-- | Pop the current buffer and create a new one with the same size as the old
-- Call this regularly to avoid excessively large buffers, but keep in mind that
-- once the buffer is popped, those characters can no longer be written to
popBuf :: Render ()
popBuf = do
  Env {printBuf} <- ask
  State {maxCoord = V2 _ mh, buf} <- get
  printBuf buf
  (_upperLeft, V2 w h) <- liftIO (getBounds buf) -- XXX upperLeft must be (0, 0)
  next <- liftIO (newArray (0, V2 w h) ' ')
  put State {offsetY = mh + 1, buf = next, maxCoord = -1}

fitBuf :: V2 Int -> Render ()
fitBuf fin@(V2 x y) = do
  State {offsetY, buf, maxCoord} <- get
  bounds@(_, V2 w h) <- liftIO (getBounds buf)
  unless (inRange bounds fin) do
    let !sx = if x > w then 2 else 1
    let !sy = if y > h then 2 else 1
    next <- liftIO (newArray (0, V2 (sx * max x w) (sy * max y h)) ' ')
    liftIO $ forM_ (range bounds) \i -> do
      orig <- readArray buf i
      writeArray next i orig
    put State {offsetY, buf = next, maxCoord}

modifyBuf :: V2 Int -> (Char -> Char) -> Render ()
modifyBuf p@(V2 x0 y0) fn = do
  State {offsetY, buf, maxCoord} <- get
  put State {offsetY, buf, maxCoord = max maxCoord p}
  bounds <- liftIO (getBounds buf) -- XXX upperLeft must be (0, 0)
  when (y0 < offsetY) do
    error "Nice.Terminal.Render.write: Tried to write to a region that was already popped with popBuf"
  let fin = V2 x0 (y0 - offsetY)
  unless (inRange bounds fin) (fitBuf fin)
  liftIO do
    ex <- readArray buf fin
    writeArray buf fin $! fn ex
