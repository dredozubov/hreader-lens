{-# LANGUAGE FlexibleContexts #-}

{-|

  Optic counterparts of MonadReader combinators from the lens library.

|-}
module Control.Lens.HReader where

import Control.Lens
import Control.Monad.HReader
import Data.HSet

hreader :: (MonadHReader m, HGettable (MHRElements m) s) => (s -> a) -> m a
hreader f = do
  e <- hask
  return $ f e

hasks :: (MonadHReader m, HGettable (MHRElements m) s) => (s -> a) -> m a
hasks = hreader

hview :: (MonadHReader m, HGettable (MHRElements m) s) => Getting a s a -> m a
hview l = hasks (getConst . l Const)
{-# INLINE hview #-}

hviews
  :: (MonadHReader m, HGettable (MHRElements m) s)
  => LensLike' (Const r) s a -> (a -> r) -> m r
hviews l f = hasks (getConst . l (Const . f))
{-# INLINE hviews #-}

hiview
  :: (MonadHReader m, HGettable (MHRElements m) s)
  => IndexedGetting i (i,a) s a -> m (i,a)
hiview l = hasks (getConst . l (Indexed $ \i -> Const . (,) i))
{-# INLINE hiview #-}
