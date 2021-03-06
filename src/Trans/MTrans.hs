{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Trans.MTrans (

    Composer'

  , pure2
  , fmap2
  , app2
  , join2
  
  , MonadTrans'(..)
  
  , MaybeT(..)
  , MonadMaybe(..)

  , WriterT(..)
  , say
  , MonadWriter(..)

  , StateT(..)
  , MonadState(..)
  , update
  
  , ReaderT(..)
  , MonadReader(..)

  , ErrorT(..)
  , MonadError(..)
  
  , ListT(..)

) where

import Classes
import Datums
import Instances
import Combinators ((*>))
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

update :: MonadState s m => (s -> s) -> m ()
update f = get >>= (put . f)


class (Monad' m, Monoid' w) => MonadWriter w m | m -> w where
  write :: w -> m ()


class Monad' m => MonadReader r m | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a


class Monad' m => MonadMaybe m where
  none :: m a


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

instance (Applicative' m) => APlus' (MaybeT m) where
  -- MaybeT m a -> MaybeT m a -> MaybeT m a
  -- m (Maybe a) -> m (Maybe a) -> m (Maybe a)
  MaybeT l  <+>  MaybeT r   =  MaybeT (fmap (<+>) l <*> r)

instance (Pointed' m, Applicative' m) => AZero' (MaybeT m) where
  -- MaybeT m a
  -- m (Maybe a)
  zero = MaybeT (pure Nothing)

instance Monad' m => AOr' (MaybeT m) where
  -- m (Maybe a) -> m (Maybe a) -> m (Maybe a)
  MaybeT l  <||>  MaybeT r  =  MaybeT q
    where
      q = l >>= \x -> case x of
                           Nothing  ->  r
                           Just y   ->  pure x

instance Monad' m => Switch' (MaybeT m) where
  -- MaybeT m a -> MaybeT m ()
  -- m (Maybe a) -> m (Maybe ())
  switch (MaybeT m) = MaybeT q 
    where
      q = m >>= (pure . switch)




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

instance APlus' m => APlus' (WriterT w m) where
  -- WriterT w m a -> WriterT w m a -> WriterT w m a
  -- m (w, a) -> m (w, a) -> m (w, a)
  WriterT x  <+>  WriterT y  =  WriterT (x <+> y)
  
instance AZero' m => AZero' (WriterT w m) where
  zero = WriterT zero

instance (Switch' m, Functor' m, Monoid' w) => Switch' (WriterT w m) where
  -- m (w, a) -> m (w, ())
  switch (WriterT m) = WriterT (fmap (const (empty, ())) (switch m))

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

instance AOr' m => AOr' (StateT s m) where
  StateT l  <||>  StateT r  =  StateT (\s -> l s <||> r s)

instance (Functor' m, Switch' m) => Switch' (StateT s m) where
  -- StateT s m a -> StateT s m ()
  -- (s -> m (s, a)) -> s -> m (s, ())
  switch (StateT f) = StateT g
    where
      g s = fmap (const (s, ())) (switch (f s))




newtype ReaderT r m a
    = ReaderT {getReaderT :: r -> m a}

instance Functor' m => Functor' (ReaderT r m) where
  -- (a -> b) -> (r -> m a) -> (r -> m b)
  fmap f (ReaderT g) = ReaderT (fmap f . g)

instance Pointed' m => Pointed' (ReaderT r m) where
  -- a -> r -> m a
  pure x = ReaderT (pure . const x)

instance (Pointed' m, Applicative' m) => Applicative' (ReaderT r m) where
  -- (r -> m (a -> b)) -> (r -> m a) -> (r -> m b)
  ReaderT f <*> ReaderT x = ReaderT (\r -> f r <*> x r)

instance Monad' m => Monad' (ReaderT r m) where
  -- (r -> m (r -> m a)) -> r -> m a
  join (ReaderT m) = ReaderT f
    where
      f r = 
          m r >>= \(ReaderT g) -> 
          g r

instance Semigroup' (m a) => Semigroup' (ReaderT r m a) where
  -- (r -> m a) -> (r -> m a) -> (r -> m a)
  ReaderT f <|> ReaderT g = ReaderT (\r -> f r <|> g r)

instance Monoid' (m a) => Monoid' (ReaderT r m a) where
  -- r -> m a
  empty = ReaderT (const empty)

instance APlus' m => APlus' (ReaderT r m) where
  -- (r -> m a) -> (r -> m a) -> (r -> m a)
  ReaderT f <+> ReaderT g  =  ReaderT (\r -> f r <+> g r)

instance AZero' m => AZero' (ReaderT r m) where
  -- r -> m a
  zero = ReaderT (const zero)

instance AOr' m => AOr' (ReaderT r m) where
  ReaderT x  <||>  ReaderT y  =  ReaderT (\r -> x r <||> y r)

instance (Switch' m, Functor' m) => Switch' (ReaderT r m) where
  -- (r -> m a) -> (r -> m ())
  switch (ReaderT f) = ReaderT (\r -> fmap (const ()) (switch $ f r))




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

instance APlus' m => APlus' (ErrorT e m) where
  ErrorT l  <+>  ErrorT r  =  ErrorT (l <+> r)

instance AZero' m => AZero' (ErrorT e m) where
  zero = ErrorT zero

-- left bias success, if they're both successful
-- left bias failure, if they both fail
instance (APlus' m, Monad' m) => AOr' (ErrorT e m) where
  ErrorT l  <||>  ErrorT r  =  ErrorT x
    where 
      x = l >>= \y -> case y of
                           Left _ -> r >>= \z -> case z of
                                                       Left _  ->  pure y;
                                                       Right _ ->  pure z;
                           Right _ -> pure y;

instance (Functor' m, Switch' m) => Switch' (ErrorT e m) where
  -- m (Either e a) -> m (Either e ())
  switch (ErrorT m) = ErrorT (fmap (const (Right ())) $ switch m)




newtype ListT m a
    = ListT {getListT :: m [a]}

instance Composer' ListT [] where
  open = getListT
  close = ListT

instance Functor' m => Functor' (ListT m) where
  fmap = fmap2

instance Pointed' m => Pointed' (ListT m) where
  pure = pure2

instance (Pointed' m, Applicative' m) => Applicative' (ListT m) where
  (<*>) = app2

-- PROBLEM:  this fails the monad associativity law if the 
--   wrapped monad is not commutative
-- SOLUTION:  if order doesn't matter, then the law is not broken
instance Monad' m => Monad' (ListT m) where
  join = join2

instance Applicative' m => APlus' (ListT m) where
  -- ListT m a -> ListT m a -> ListT m a
  -- m [a] -> m [a] -> m [a]
  ListT l  <+>  ListT r  =  ListT (fmap (++) l <*> r)

instance (Pointed' m, Applicative' m) => AZero' (ListT m) where
  -- m [a]
  zero = ListT (pure [])

instance Monad' m => AOr' (ListT m) where
  ListT l  <||>  ListT r  =  ListT x
    where
      x = l >>= \y -> case y of
                           []  ->  r
                           _   ->  pure y

instance (Switch' m, Functor' m) => Switch' (ListT m) where
  -- m [a] -> m [()]
  switch (ListT m) = ListT (fmap (const [()]) $ switch m)

