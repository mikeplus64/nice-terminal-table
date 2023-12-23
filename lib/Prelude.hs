{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Prelude (module Relude, module Linear, module Control.Lens, forIntRange) where

import Control.Lens
import GHC.Exts (Int (..), Int#, (+#), (<#))
import Linear
import Relude hiding (identity, trace, transpose, uncons, universe, (??))

forIntRange :: forall m. (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
forIntRange (I# i0) (I# j0) f = go i0
  where
    go :: Int# -> m ()
    go i = case i <# j0 of
      1# -> f (I# i) >> go (i +# 1#)
      _ -> pure ()
