{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Trans.Instances (

) where


import Classes
import Datums
import Instances
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))
import Trans.MTrans
import Trans.Parse



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
  lift m = ErrorT (m >>= (pure . Right))

instance MonadTrans' (Parser t) where
  -- m a -> Parser t m a
  -- m a -> StateT [t] m a
  -- m a -> [t] -> m ([t], a)
  lift m = Parser h
    where
      h = StateT (\ts -> m >>= \x -> pure (ts, x))

instance MonadTrans' CntP where
  -- m a -> CntP m a
  -- m a -> StateT (Int, Int) m a
  -- m a -> (Int, Int) -> m ((Int, Int), a)
  lift m = CntP h
    where
      h = StateT (\ints -> m >>= \x -> pure (ints, x))

instance MonadTrans' ListT where
  -- m a -> ListT m a
  -- m a -> m [a]
  lift m = ListT (m >>= \x -> pure [x])


-- ---------------------------------------------------------------------

instance (Monoid' w, Monad' m) => MonadWriter w (WriterT w m) where
  write x = pure (x, ())

instance MonadWriter w m => MonadWriter w (StateT s m) where
  -- w -> StateT s m (w, ())
  -- w -> s -> m (s, (w, ())
  write = lift . write



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

instance Monad' m => MonadState [t] (Parser t m) where
  get    =  Parser get
  put x  =  Parser (put x)

instance Monad' m => MonadState (Int, Int) (CntP m) where
  get  =  CntP  get
  put  =  CntP  .  put


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

instance MonadError e m => MonadError e (Parser t m) where
  throwE      =  lift . throwE
  catchE m f  =  Parser $ catchE (getParser m) (getParser . f)

instance MonadError e m => MonadError e (ListT m) where
  throwE      =  lift . throwE
  catchE m f  =  ListT $ catchE (getListT m) (getListT . f)


-- ---------------------------------------------------------------------

instance (AZero' m, Monad' m) => MonadParser t (Parser t m) where
  item = Parser item

-- the state is (space indentation, line number)
-- passing a '\n' resets the space to zero
-- this ignores tabs -- just lazy
instance (MonadParser Char m) => MonadParser Char (CntP m) where
  item =
      get        >>= \(ss, ns) ->
      lift item  >>= \x -> case x of
                                ' '  -> put (ss + 1, ns) >> pure x;
                                '\n' -> put (0, ns + 1)  >> pure x;
                                y    -> pure y;

instance (Monad' m, AZero' m) => MonadParser t (StateT [t] m) where
  item = 
      get >>= \xs -> case xs of
                          (y:ys)  ->  put ys >> pure y;
                          []      ->  zero;