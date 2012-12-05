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
  
  , ErrorT(..)
  , MonadError(..)

) where

import Classes
import Datums
import Instances
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))


-- why did I put f and g in this order?  does it matter?
class Composer' c g | c -> g where
  open  :: c f a -> f (g a)
  close :: f (g a) -> c f a


class MonadTrans' t where
  lift :: Monad' m => m a -> t m a


class Monad' m => MonadError e m | m -> e where
  throwE :: e -> m a 
  catchE :: m a -> (e -> m a) -> m a


class Monad' m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()


class (Monad' m, Monoid' w) => MonadWriter w m | m -> w where
  write :: w -> m (w, ())


-- ---------------------------------------------------------------------

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


-- ---------------------------------------------------------------------

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

instance APlus' m => APlus' (MaybeT m) where
  -- MaybeT m a -> MaybeT m a -> MaybeT m a
  -- m (Maybe a) -> m (Maybe a) -> m (Maybe a)
  MaybeT l  <+>  MaybeT r   =  MaybeT (l <+> r)

instance AZero' m => AZero' (MaybeT m) where
  -- MaybeT m a
  -- m (Maybe a)
  zero = MaybeT zero

instance IsZero' m => IsZero' (MaybeT m) where
  -- MaybeT m a -> Bool
  -- m (Maybe a) -> Bool
  isZero (MaybeT m)  =  isZero m



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

instance APlus' m => APlus' (StateT s m) where
  -- (s -> m (s, a)) -> (s -> m (s, a)) -> s -> m (s, a)
  StateT f  <+>  StateT g  =  StateT (\s -> f s <+> g s)
  
instance AZero' m => AZero' (StateT s m) where
  -- s -> m (s, a)
  zero = StateT (const zero)



newtype ErrorT e m a
    = ErrorT {getErrorT :: m (Either e a)}

instance Composer' (ErrorT e) (Either e) where
  -- ErrorT m a -> m (Either e a)
  open = getErrorT
  -- m (Either e a) -> ErrorT m a
  close = ErrorT

instance Functor' m => Functor' (ErrorT e m) where
  fmap = fmap2

instance Pointed' m => Pointed' (ErrorT e m) where
  pure = pure2

instance (Pointed' m, Applicative' m) => Applicative' (ErrorT e m) where
  (<*>) = app2

instance (Monad' m) => Monad' (ErrorT e m) where
  join = join2
