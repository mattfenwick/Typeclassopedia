module Classes (

    Functor'
  , fmap

  , Pointed'
  , pure

  , Applicative'
  , (<*>)

  , Alternative'
  , empty
  , (<|>)
  , guard

  , Alt'
  , fempty
  , (<<>>)

  , Monoid'
  , mempty
  , mappend
  , mconcat

  , Monad'
  , join
  , (>>=)
  , (>>)

) where

import Prelude hiding (fmap, (>>=), (>>), fail)




class Functor' f where
  fmap :: (a -> b) -> f a -> f b
  
  
class Pointed' f where
  pure :: a -> f a
  

{-  
class Comonad' m where
-}


class Monoid' a where
  mempty  :: a
  mappend :: a -> a -> a

  
class (Functor' f) => Applicative' f where
  (<*>) :: f (a -> b) -> f a -> f b


-- why do we need the Applicative' constraint?
-- why not just: (<|>) :: a -> a -> a ??
class (Applicative' f) => Alternative' f where
  empty :: f a
  (<|>) :: f a -> f a -> f a


class Alt' a where
  fempty :: a
  (<<>>) :: a -> a -> a
  
  
class (Applicative' m) => Monad' m where
  join :: m (m a) -> m a


-- wait ... if it's a Monad, 
--   and it's an alternative, then
--   isn't it automatically a MonadPlus?
-- i.e. (Alternative' m, Monad' m) => ...
class (Monad' m) => MonadPlus' m where
  mzero :: m a
  mplus :: m a -> m a -> m a
  
  
-- -------------------------------
-- some more combinators

mconcat :: Monoid' a => [a] -> a
mconcat = foldr mappend mempty


(>>=) :: Monad' m => m a -> (a -> m b) -> m b  
m >>= f = join (fmap f m)


(>>) :: Monad' m => m a -> m b -> m b
m >> f = m >>= const f


guard :: (Pointed' m, Alternative' m) => Bool -> m ()
guard True = pure ()
guard False = empty
  
  
-- -------------------------------
-- some instances

instance Functor' [] where
  fmap _ []       = []
  fmap f (x:xs)   = f x : fmap f xs
  
  
instance Pointed' [] where
  pure x = [x]


instance Monoid' [a] where
  mempty = []
  mappend = (++)
  
  
instance Applicative' [] where
  fs <*> xs = concat $ fmap (\f -> fmap f xs) fs


instance Alternative' [] where
  empty = []
  (<|>) = (++)


instance Alt' [a] where
  fempty = []
  (<<>>) = (++)


instance Monad' [] where
  join = concat


instance MonadPlus' [] where
  mzero = []
  mplus = (++)



instance Alt' Bool where
  fempty = False
  False <<>> x = x
  x <<>> _     = x
  

  
  
instance Functor' IO where
  fmap f x = do { x' <- x; return (f x')}
  
  
instance Pointed' IO where
  pure = return
  
  
instance Applicative' IO where
  fs <*> xs = do {f <- fs; x <- xs; return (f x)}
  
  
instance Monad' IO where
  join m = do {x <- m; y <- x; return y}



instance Functor' Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)


instance Monoid' a => Monoid' (Maybe a) where
  mempty = Nothing
  Nothing `mappend` b = b
  a `mappend` Nothing = a
  (Just a) `mappend` (Just b) = Just (a `mappend` b)


instance Pointed' Maybe where
  pure = Just


instance Applicative' Maybe where
  Nothing  <*>    _     = Nothing
  _        <*> Nothing  = Nothing
  (Just f) <*> (Just x) = Just (f x)


instance Alternative' Maybe where
  empty = Nothing
  Nothing <|> x = x
  x <|> _       = x


instance Monad' Maybe where
  join (Just (Just x))  = Just x
  join      _           = Nothing








