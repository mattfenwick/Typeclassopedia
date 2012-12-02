{-# LANGUAGE NoMonomorphismRestriction #-}
module MTrans (

) where

import Classes
import Datums
import Instances
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))


class Composer c where
  open :: c f g a -> f (g a)   -- pure?
  close :: f (g a) -> c f g a  -- extract?


pure2 :: (Pointed' f, Pointed' g) => a -> f (g a)
pure2 = pure . pure

pure2' :: (Pointed' f, Pointed' g) => (f (g a) -> x) -> a -> x
pure2' close = close . pure . pure

pure2'' :: (Pointed' f, Pointed' g, Composer c) => a -> c f g a
pure2'' = close . pure . pure

fmap2 :: (Functor' f, Functor' g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

fmap2' open close f = close . fmap (fmap f) . open

fmap2'' :: (Functor' f, Functor' g, Composer c) => (a -> b) -> c f g a -> c f g b
fmap2'' f = close . fmap (fmap f) . open

app2 :: (Pointed' f, Applicative' f, Applicative' g) => 
    f (g (a -> b))
 -> f (g a)
 -> f (g b)
app2 f x = pure (<*>) <*> f <*> x

app2' open1 open2 close f x = close (pure (<*>) <*> open1 f <*> open2 x)

-- note that this type could actually be more general
app2'' :: (Pointed' f, Applicative' f, Applicative' g, Composer c) => c f g (a -> b) -> c f g a -> c f g b
app2'' f x = close (pure (<*>) <*> open f <*> open x)

join2 :: (Monad' m, Monad' n, Traversable' n) => m (n (m (n a))) -> m (n a)
join2 =
    fmap join      .
    join           .
    fmap commute

join2' open1 open2 close = 
    close              . 
    fmap join          . 
    join               . 
    fmap commute       . 
    fmap (fmap open2)  . 
    open1


class MonadTrans' t where
  lift :: Monad' m => m a -> t m a



newtype MaybeT m a
    = MaybeT {getMaybeT :: m (Maybe a)}

instance Functor' m => Functor' (MaybeT m) where
  fmap f = MaybeT . fmap (fmap f) . getMaybeT

instance Pointed' m => Pointed' (MaybeT m) where
  -- a -> MaybeT m a
  -- a -> m (Maybe a)
  pure = MaybeT . pure . pure

instance (Pointed' m, Applicative' m) => Applicative' (MaybeT m) where
  -- MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  -- m (Maybe (a -> b)) -> m (Maybe a) -> m (Maybe b)
  MaybeT f <*> MaybeT x = MaybeT (app2 f x)

instance Monad' m => Monad' (MaybeT m) where
  -- MaybeT m (MaybeT m a) -> MaybeT m a
  -- m (Maybe (m (Maybe a))) -> m (Maybe a)
  join = join2' getMaybeT getMaybeT MaybeT
  -- this is so ghetto b/c I don't know how
  -- to use higher-rank types

instance MonadTrans' MaybeT where
  -- m a -> MaybeT m a
  -- m a -> m (Maybe a)
  lift = MaybeT . fmap Just



newtype WriterT w m a
    = WriterT {getWriterT :: m (w, a)}

instance Functor' m => Functor' (WriterT w m) where
  fmap = fmap2' getWriterT WriterT

instance (Monoid' w, Pointed' m) => Pointed' (WriterT w m) where
  pure = pure2' WriterT

