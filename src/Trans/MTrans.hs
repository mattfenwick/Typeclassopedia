{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Trans.MTrans (

    Composer'

  , pure2
  , fmap2
  , app2
  , join2
  
  , MonadTrans'(..)
  
  , MaybeT(..)

  , WriterT(..)
  , say
  , MonadWriter(..)

  , StateT(..)
  , MonadState(..)

) where

import Classes
import Datums
import Instances
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))


-- why did I put f and g in this order?  does it matter?
class Composer' c g | c -> g where
  open  :: c f a -> f (g a)
  close :: f (g a) -> c f a


pure2 :: (Pointed' f, Pointed' g, Composer' c g) => a -> c f a
pure2 = close . pure . pure

fmap2 :: (Functor' f, Functor' g, Composer' c g) => (a -> b) -> c f a -> c f b
fmap2 f = close . fmap (fmap f) . open

-- note that this type could actually be more general
app2 :: (Pointed' f, Applicative' f, Applicative' g, Composer' c g) => c f (a -> b) -> c f a -> c f b
app2 f x = close (pure (<*>) <*> open f <*> open x)

join2 :: (Traversable' g, Monad' g, Monad' f, Composer' c g) => c f (c f a) -> c f a
join2 = 
    close              .
    fmap join          .
    join               .
    fmap commute       .
    fmap (fmap open)   .
    open


class MonadTrans' t where
  lift :: Monad' m => m a -> t m a



-- (* -> *) -> * -> *
newtype MaybeT m a
    = MaybeT {getMaybeT :: m (Maybe a)}

instance Composer' MaybeT Maybe where
  open   =  getMaybeT
  close  =  MaybeT

instance Show (m (Maybe a)) => Show (MaybeT m a) where
  show = show . getMaybeT

instance Functor' m => Functor' (MaybeT m) where
  -- (a -> b) -> MaybeT m a -> MaybeT m b
  fmap = fmap2

instance Pointed' m => Pointed' (MaybeT m) where
  -- a -> MaybeT m a
  -- a -> m (Maybe a)
  pure = pure2

instance (Pointed' m, Applicative' m) => Applicative' (MaybeT m) where
  -- MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  -- m (Maybe (a -> b)) -> m (Maybe a) -> m (Maybe b)
  (<*>) = app2

instance Monad' m => Monad' (MaybeT m) where
  -- MaybeT m (MaybeT m a) -> MaybeT m a
  -- m (Maybe (m (Maybe a))) -> m (Maybe a)
  join = join2

instance MonadTrans' MaybeT where
  -- m a -> MaybeT m a
  -- m a -> m (Maybe a)
  lift = MaybeT . fmap Just



-- * -> (* -> *) -> * -> *
newtype WriterT w m a
    = WriterT {getWriterT :: m (w, a)}

instance Show (m (w, a)) => Show (WriterT w m a) where
  show = show . getWriterT

instance Composer' (WriterT w) ((,) w) where
  open   =  getWriterT
  close  =  WriterT

instance Functor' m => Functor' (WriterT w m) where
  fmap = fmap2

instance (Monoid' w, Pointed' m) => Pointed' (WriterT w m) where
  pure = pure2

instance (Applicative' m, Pointed' m, Semigroup' w) => Applicative' (WriterT w m) where
  (<*>) = app2

instance (Monad' m, Monoid' w) => Monad' (WriterT w m) where
  join = join2

-- instance Semigroup' m => Semigroup' (WriterT w m) where
--  WriterT x <|> WriterT y = 

say :: Pointed' m => w -> WriterT w m ()
-- say :: w -> (w, ())
-- say l = (l, ())
say l = WriterT (pure (l, ()))

instance Monoid' w => MonadTrans' (WriterT w) where
  -- m a -> WriterT w m a
  -- m a -> WriterT (m (w, a))
  -- m a -> m (w, a)
  lift m = WriterT (m >>= \x -> pure (empty, x))

class (Monad' m, Monoid' w) => MonadWriter w m | m -> w where
  write :: w -> m (w, ())

instance (Monoid' w, Monad' m) => MonadWriter w (WriterT w m) where
  write x = pure (x, ())

instance MonadWriter w m => MonadWriter w (StateT s m) where
  -- w -> StateT s m (w, ())
  -- w -> s -> m (s, (w, ())
--  write x = 
  write = lift . write



-- * -> (* -> *) -> * -> *
newtype StateT s m a = StateT {getStateT :: s -> m (s, a)}

-- instance Composer' (StateT s) where
  
instance Functor' m => Functor' (StateT s m) where
  fmap f (StateT g) = StateT (fmap (fmap f) . g)

instance Pointed' m => Pointed' (StateT s m) where
  pure x = StateT (\s -> pure (s, x))

-- does this require Monad?  apparently:
--  http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#v:StateT
instance Monad' m => Applicative' (StateT s m) where
  StateT f <*> StateT x = StateT h
    where
      h s1 =
          f s1 >>= \(s2, f') ->
          x s2 >>= \(s3, x') ->
          pure (s3, f' x')

instance Monad' m => Monad' (StateT s m) where
  -- StateT s m (StateT s m a) -> StateT s m a
  -- (s -> m (s, s -> m (s, a))) -> s -> m (s, a)
  join (StateT v1) = StateT h
    where
      h s1 = 
          v1 s1 >>= \(s2, StateT v2) ->
          v2 s2

instance Semigroup' (m (s, a)) => Semigroup' (StateT s m a) where
  StateT f <|> StateT g = StateT (\xs -> f xs <|> g xs)

instance Monoid' (m (s, a)) => Monoid' (StateT s m a) where
  empty = StateT (const empty)

instance MonadTrans' (StateT s) where
  -- Monad' m => m a -> StateT s m a
  -- m a -> s -> m (s, a)
  lift m = StateT h
    where
      h s = 
          m >>= \x -> 
          pure (s, x)

class Monad' m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

instance MonadState s (State s) where
  get  =  fetch
  put  =  set

instance Monad' m => MonadState s (StateT s m) where
  get    =  StateT (\s -> pure (s, s))
  put s  =  StateT (\_ -> pure (s, ()))

-- does this overlap with the next instance below? ???
instance MonadState s m => MonadState s (MaybeT m) where
  get  =  lift get
  put  =  lift . put

instance (MonadTrans' t, MonadState s m, Monad' (t m)) => MonadState s (t m) where
  -- m s
  -- s -> (s, s)
  get  =  lift get
  -- s -> m ()
  -- s -> t -> (s, ())
  put  =  lift . put
