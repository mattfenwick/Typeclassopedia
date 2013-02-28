{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, 
             FlexibleInstances, UndecidableInstances,
             NoMonomorphismRestriction #-}


-- data definitions

newtype MaybeT m a =
    MaybeT {getMaybeT :: m (Maybe a)}

newtype StateT s m a =
    StateT {getStateT :: s -> m (s, a)}

newtype ErrorT e m a =
    ErrorT {getErrorT :: m (Either e a)}

newtype Id a =
    Id {getId :: a}


-- transformer type classes

class Monad m => TMaybe m where
  zero :: m a

class Monad m => TState s m | m -> s where
  get :: m s
  put :: s -> m ()

class Monad m => TError e m | m -> e where
  throwE :: e -> m a
  catchE :: m a -> (e -> m a) -> m a
  
  
-- monad transformer type class

class Trans t m where
  lift :: Monad m => m a -> t m a


-- monad instances

instance Monad Id where
  return = Id
  Id x >>= f = f x

instance Monad m => Monad (MaybeT m) where
  -- a -> m (Maybe a)
  return = MaybeT . return . Just
  -- m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
  MaybeT x >>= f = MaybeT (x >>= g)
    where
      g Nothing  = return Nothing 
      g (Just z) = getMaybeT (f z)

instance Monad m => Monad (StateT s m) where
  -- a -> s -> m (s, a)
  return x = StateT (\s -> return (s, x))
  -- (s -> m (s, a)) -> (a -> s -> m (s, b)) -> s -> m (s, b)
  x >>= f = StateT y
    where
      y s =  
          getStateT x s >>= \(s', a) -> 
          getStateT (f a) s'

instance Monad m => Monad (ErrorT e m) where
  -- a -> m (Either e a)
  return = ErrorT . return . Right
  -- m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
  x >>= f = ErrorT (getErrorT x >>= g)
    where
      g (Left e)   =  return (Left e)
      g (Right z)  =  getErrorT (f z)


-- monad transformer instances

instance Trans MaybeT m where
  -- m a -> m (Maybe a)
  lift m = MaybeT (m >>= return . Just)

instance Trans (StateT s) m where
  -- m a -> (s -> m (s, a))
  lift m = StateT (\s -> m >>= \a -> return (s, a))

instance Trans (ErrorT e) m where
  -- m a -> m (Error e a)
  lift m = ErrorT (m >>= return . Right)


-- TMaybe instances

instance Monad m => TMaybe (MaybeT m) where
  -- m (Maybe a)
  zero = MaybeT (return Nothing)

instance TMaybe m => TMaybe (StateT s m) where
  zero = lift zero

instance TMaybe m => TMaybe (ErrorT e m) where
  zero = lift zero


-- TState instances

instance Monad m => TState s (StateT s m) where
  -- s -> m (s, a)
  get = StateT (\s -> return (s, s))
  -- s -> s -> m (s, ())
  put s = StateT (\_ -> return (s, ()))

instance TState s m => TState s (MaybeT m) where
  get = lift get
  put = lift . put

instance TState s m => TState s (ErrorT e m) where
  get = lift get
  put = lift . put


-- TError instances

instance Monad m => TError e (ErrorT e m) where
  -- e -> m (Either e a)
  throwE = ErrorT . return . Left
  -- m (Either e a) -> (e -> m (Either e a)) -> m (Either e a)
  catchE err f = ErrorT (getErrorT err >>= g)
    where
      g (Left e)  = getErrorT (f e)
      g (Right z) = return (Right z)

instance TError e m => TError e (MaybeT m) where
  throwE = lift . throwE 
  catchE m f = MaybeT $ catchE (getMaybeT m) (getMaybeT . f)

instance TError e m => TError e (StateT s  m) where
  throwE = lift . throwE
  catchE m f = StateT (\s -> catchE (getStateT m s) (\e -> getStateT (f e) s))


-- examples

item = 
    get  >>= \ts -> case ts of
                         (x:xs) -> put xs >> return x;
                         []     -> zero;
                         
simple :: StateT [t] (MaybeT Id) t
simple = item

runSimple :: StateT [t] (MaybeT Id) a -> [t] -> Maybe ([t], a)
runSimple p = getId . getMaybeT . getStateT p


addError :: StateT [t] (MaybeT (ErrorT e Id)) t
addError = item

err = throwE "oops -- an error !"

runAE :: StateT [t] (MaybeT (ErrorT e Id)) a -> [t] -> Either e (Maybe ([t], a))
runAE p toks = getId . getErrorT . getMaybeT . getStateT p $ toks


type Parser t s e a = StateT [t] (StateT s (MaybeT (ErrorT e Id))) a

addState :: Parser t s e t
addState = item

getTokens :: Parser t s e [t]
getTokens = get

putTokens :: [t] -> Parser t s e ()
putTokens = put

getState :: Parser t s e s
getState = lift get

putState :: s -> Parser t s e ()
putState = lift . put

runParser :: Parser t s e a 
      -> [t]
      -> s
      -> Either e (Maybe (s, ([t], a)))
runParser p toks = getId . getErrorT . getMaybeT . getStateT (getStateT p toks)

check p = 
    item >>= \x ->
    if p x then return x
           else zero

literal = check . (==)

open = 
    literal '{'  >>
    getState     >>= 
    putState . ((:) '{')

close = 
    literal '}'  >>
    getState     >>= \s ->
        case s of ('{':ts) -> putState ts;
                  _       -> throwE "unmatched }"