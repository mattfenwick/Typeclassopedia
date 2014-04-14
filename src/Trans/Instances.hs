{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Trans.Instances (

) where


import Classes
import Datums (State(..), set, fetch)
import Instances
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))
import Trans.MTrans



-- ---------------------------------------------------------------------

instance MonadTrans' MaybeT where
  -- m a -> MaybeT m a
  -- m a -> m (Maybe a)
  lift = MaybeT . fmap Just

instance Monoid' w => MonadTrans' (WriterT w) where
  -- m a -> WriterT w m a
  -- m a -> WriterT (m (w, a))
  -- m a -> m (w, a)
  lift m = WriterT (m >>= \x -> pure (empty, x))

instance MonadTrans' (StateT s) where
  -- Monad' m => m a -> StateT s m a
  -- m a -> s -> m (s, a)
  lift m = StateT h
    where
      h s = 
          m >>= \x -> 
          pure (s, x)

instance MonadTrans' (ErrorT e) where
  -- m a -> ErrorT e m a
  -- m a -> m (Either e a)
  lift = ErrorT . fmap Right

instance MonadTrans' ListT where
  -- m a -> ListT m a
  -- m a -> m [a]
  lift = ListT . fmap (:[])

instance MonadTrans' (ReaderT r) where
  -- m a -> ReaderT r m a
  -- m a -> (r -> m a)
  lift = ReaderT . const


-- ---------------------------------------------------------------------

instance (Monoid' w, Monad' m) => MonadWriter w (WriterT w m) where
  -- w -> m (w, ())
  write x = WriterT (pure (x, ()))

instance MonadWriter w m => MonadWriter w (StateT s m) where
  -- w -> StateT s m (w, ())
  -- w -> s -> m (s, (w, ())
  write = lift . write
  
instance MonadWriter w m => MonadWriter w (MaybeT m) where
  write = lift . write 

instance MonadWriter w m => MonadWriter w (ReaderT r m) where
  write = lift . write

instance MonadWriter w m => MonadWriter w (ErrorT e m) where
  write = lift . write

instance MonadWriter w m => MonadWriter w (ListT m) where
  write = lift . write


-- ---------------------------------------------------------------------

instance MonadMaybe Maybe where
  none          = Nothing

instance Monad' m => MonadMaybe (MaybeT m) where
  -- m (Maybe a)
  none            = MaybeT (pure Nothing)

instance MonadMaybe m => MonadMaybe (StateT s m) where
  none = lift none
  
instance MonadMaybe m => MonadMaybe (ErrorT e m) where
  none = lift none

instance MonadMaybe m => MonadMaybe (ReaderT r m) where
  none = lift none

instance (MonadMaybe m, Monoid' w) => MonadMaybe (WriterT w m) where
  none = lift none

instance MonadMaybe m => MonadMaybe (ListT m) where
  none = lift none

-- ---------------------------------------------------------------------

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

{-
instance (MonadTrans' t, MonadState s m, Monad' (t m)) => MonadState s (t m) where
  -- m s
  -- s -> (s, s)
  get  =  lift get
  -- s -> m ()
  -- s -> t -> (s, ())
  put  =  lift . put
-}

instance MonadState s m => MonadState s (ReaderT r m) where
  get = lift get
  put = lift . put

instance MonadState s m => MonadState s (ErrorT e m) where
  get = lift get
  put = lift . put

instance MonadState s m => MonadState s (ListT m) where
  get = lift get
  put = lift . put

instance (MonadState s m, Monoid' w) => MonadState s (WriterT w m) where
  get = lift get
  put = lift . put


-- ---------------------------------------------------------------------

instance Monad' m => MonadError e (ErrorT e m) where
  -- e -> ErrorT e m a
  -- e -> m (Either e a)
  throwE = ErrorT . pure . Left
  -- m a -> (e -> m a) -> m a
  -- ErrorT e m a -> (e -> ErrorT e m a) -> ErrorT e m a
  catchE (ErrorT m) f  =  
      ErrorT (m >>= \x -> case x of
                            Left e   ->  getErrorT (f e) -- <== um, seriously?
                            Right z  ->  pure $ Right z)

instance MonadError e (Either e) where
  -- e -> Either e a
  throwE = Left
  -- Either e a -> (e -> Either e a) -> Either e a
  catchE (Right x)  _  =  Right x
  catchE (Left y)   f  =  f y

instance MonadError e m => MonadError e (MaybeT m) where
  throwE  =  lift . throwE
  -- from:  m a -> (e -> m a) -> m a
  -- to:    MaybeT m a -> (e -> MaybeT m a) -> MaybeT m a
  -- TODO:  figure out what this code does, and how it works
  -- (got it from http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/src/Control-Monad-Trans-Maybe.html#liftCatch)
  catchE m f = MaybeT $ catchE (getMaybeT m) (getMaybeT . f)

instance MonadError e m => MonadError e (StateT s m) where
  throwE      =  lift . throwE
  -- m a -> (e -> m a) -> m a
  -- StateT s m a -> (e -> StateT s m a) -> StateT s m a
  -- (s -> m (s, a)) -> (e -> s -> m (s, a)) -> s -> m (s, a)
  -- http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/src/Control-Monad-Trans-State-Lazy.html#liftCatch
  catchE m f  =  StateT (\s -> catchE (getStateT m s) (\e -> getStateT (f e) s))

instance MonadError e m => MonadError e (ListT m) where
  throwE      =  lift . throwE
  catchE m f  =  ListT $ catchE (getListT m) (getListT . f)

instance MonadError e m => MonadError e (ReaderT r m) where
  throwE      =  lift . throwE
  catchE m f  =  ReaderT (\r -> catchE (getReaderT m r) (\e -> getReaderT (f e) r))

instance (MonadError e m, Monoid' w) => MonadError e (WriterT w m) where
  throwE      =  lift . throwE
  catchE m f  =  WriterT $ catchE (getWriterT m) (getWriterT . f)


-- ---------------------------------------------------------------------

instance Monad' m => MonadReader r (ReaderT r m) where
  -- r -> m r
  ask                 = ReaderT pure
  local f (ReaderT g) = ReaderT (g . f)

-- ErrorT
instance MonadReader r m => MonadReader r (ErrorT e m) where
  -- ErrorT e m a
  -- m (Either e a)
  ask = lift ask
  -- (r -> r) -> m a -> m a
  -- (r -> r) -> ErrorT e m a -> ErrorT e m a
  local f (ErrorT m) = ErrorT (local f m)

instance MonadReader r m => MonadReader r (MaybeT m) where
  ask = lift ask
  local f (MaybeT m) = MaybeT (local f m)

instance MonadReader r m => MonadReader r (StateT s m) where
  ask = lift ask
  -- (r -> r) -> (s -> m (s, a)) -> (s -> m (s, a))
  local = \f m -> StateT (\s -> local f (getStateT m s))

instance MonadReader r m => MonadReader r (ListT m) where
  ask = lift ask
  local f (ListT m) = ListT (local f m)

instance (MonadReader r m, Monoid' w) => MonadReader r (WriterT w m) where
  ask = lift ask
  local f (WriterT m) = WriterT (local f m)

