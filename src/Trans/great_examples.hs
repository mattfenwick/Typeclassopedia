{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances,
             UndecidableInstances,
             NoMonomorphismRestriction #-}


-- data definitions

newtype Id a =
    Id {getId :: a}

newtype MaybeT m a =
    MaybeT {getMaybeT :: m (Maybe a)}

newtype StateT s m a =
    StateT {getStateT :: s -> m (s, a)}

newtype ErrorT e m a =
    ErrorT {getErrorT :: m (Either e a)}


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


-- monad transformer type class

class Trans t m where
  lift :: Monad m => m a -> t m a


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


-- transformer type classes

class Monad m => TMaybe m where
  zero :: m a

class Monad m => TState s m | m -> s where
  get :: m s
  put :: s -> m ()

class Monad m => TError e m | m -> e where
  throwE :: e -> m a
  catchE :: m a -> (e -> m a) -> m a
  
  
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


class Plus m where
  (<+>) :: m a -> m a -> m a
 
instance Plus Maybe where
  Nothing  <+>   a   =   a
  b        <+>   _   =   b
 
instance Monad m => Plus (MaybeT m) where
  -- m (Maybe a) -> m (Maybe a) -> m (Maybe a)
  MaybeT l  <+>  MaybeT r  =  MaybeT x
    where
      x = l >>= \y -> case y of Nothing -> r;
                                Just _  -> return y;

instance Plus m => Plus (StateT s m) where
  -- (s -> m (s, a)) -> (s -> m (s, a)) -> (s -> m (s, a))
  StateT f  <+>  StateT g  =  StateT (\s -> f s <+> g s)

instance Plus m => Plus (ErrorT e m) where
  -- m (Either e a) -> m (Either e a) -> m (Either e a)
  ErrorT l  <+>  ErrorT r  =  ErrorT (l <+> r)

instance Plus Id where
  x <+> _ = x

pany :: (Plus f, TMaybe f) => [f a] -> f a
pany = foldl (<+>) zero

class Switch f where
  switch :: f a -> f ()

instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing  = Just ()

instance Monad m => Switch (MaybeT m) where
  -- MaybeT m a -> MaybeT m ()
  -- m (Maybe a) -> m (Maybe ())
  switch (MaybeT m) = MaybeT q 
    where
      q = m >>= \x -> case x of Nothing -> return (Just ());
                                Just _  -> return Nothing;
                                
instance (Monad m, Switch m) => Switch (StateT s m) where
  -- StateT s m a -> StateT s m ()
  -- (s -> m (s, a)) -> s -> m (s, ())
  switch (StateT f) = StateT g
    where
      g s = switch (f s)    >> 
            return (s, ())

check p = 
    item >>= \x ->
    if p x then return x
           else zero

literal = check . (==)

many1 p = 
    p                       >>= \x ->
    (many1 p <+> return []) >>= \xs ->
    return (x:xs)

many0 p = many1 p <+> return []

not1 p = switch p >> item

commit p e = p <+> throwE e

mmap :: Monad m => (a -> b) -> m a -> m b -- oh look, it's fmap
mmap f m = m >>= (return . f)

a << b = a >>= \x -> b >> return x


data Thing 
    = C Char 
    | PBlock [Thing] 
    | CBlock [Thing]
    | SBlock [Thing]
  deriving (Show)

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

updateState f = getState >>= (putState . f)

runParser :: Parser t s e a 
      -> [t]
      -> s
      -> Either e (Maybe (s, ([t], a)))
runParser p toks = getId . getErrorT . getMaybeT . getStateT (getStateT p toks)

type Tok = (Char, Int, Int)

charLit :: Char -> Parser Tok s e Tok
charLit c = check (\(c', _, _) -> c == c')

open :: Char -> Parser Tok [Tok] e Tok
open c = 
    charLit c            >>= \o -> 
    updateState ((:) o)  >>
    return o

matches '(' ')' = True
matches '{' '}' = True
matches '[' ']' = True
matches _ _ = False

close :: Tok -> Parser Tok [Tok] (String, [Tok]) ()
close c =
    getState  >>= \s ->
    case (c, s) of (_, [])   -> throwE ("unmatched close brace", [c])
                   (_, t:ts) -> if matches (tok t) (tok c) then putState ts
                                                     else throwE ("mismatched braces", [t, c])

closeP :: Parser Tok [Tok] (String, [Tok]) Tok
closeP = charLit ')' >>= \s -> close s >> return s

closeC = charLit '}' >>= \s -> close s >> return s

closeS = charLit ']' >>= \s -> close s >> return s

tok :: (a, b, c) -> a
tok (c, _, _) = c

char = mmap (C . tok) $ not1 (pany [open '(', open '{', open '[', closeP, closeC, closeS])

pBlock = 
    open '(' >>= \o ->
    commit (mmap PBlock (many0 form << closeP)) ("unmatched (", [o])

cBlock =
    open '{' >>= \o ->
    commit (mmap CBlock (many0 form << closeC)) ("unmatched {", [o])
    
sBlock = 
    open '[' >>= \o ->
    commit (mmap SBlock (many0 form << closeS)) ("unmatched [", [o])

form = char <+> pBlock <+> cBlock <+> sBlock

addLineCol :: String -> [(Char, Int, Int)]
addLineCol = reverse . snd . foldl f ((1, 1), [])
  where
    f ((line, col), ts) '\n' = ((line + 1, 1), ('\n', line, col):ts)
    f ((line, col), ts)  c   = ((line, col + 1), (c, line, col):ts)

fullEg str = runParser (many0 form) (addLineCol str) []

egs = map fullEg ["ab{cd{ef}z q\nr{s\n312{4}}\n}ab",
                  "ab{123{{1342342} 325342}zzz",
                  "def{45{56}23}zr\nsss}84",
                  "ab(\ncd}fg",
                  "1(2[3)4)5"]