module Classes (

    Functor'
  , fmap

  , Pointed'
  , pure

  , Applicative'
  , (<*>)
  
  , Semigroup'
  , (<|>)

  , Monoid'
  , empty
  , mconcat
  , guard

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


class Semigroup' a where
  (<|>)  :: a -> a -> a


class (Semigroup' a) => Monoid' a where
  empty :: a

  
class (Functor' f) => Applicative' f where
  (<*>) :: f (a -> b) -> f a -> f b

  
class (Applicative' m) => Monad' m where
  join :: m (m a) -> m a
  
  
-- -------------------------------
-- some more combinators

mconcat :: Monoid' a => [a] -> a
mconcat = foldr (<|>) empty


(>>=) :: Monad' m => m a -> (a -> m b) -> m b  
m >>= f = join (fmap f m)


(>>) :: Monad' m => m a -> m b -> m b
m >> f = m >>= const f


guard :: (Pointed' m, Monoid' (m ())) => Bool -> m ()
guard True = pure ()
guard False = empty
  
  
-- -------------------------------
-- some instances


instance Functor' ((,) a) where
  fmap f (x, y) = (x, f y)
  
  


data Id a = Id a deriving (Show, Eq, Ord)

instance Functor' Id where
  fmap f (Id x) = Id (f x)
  
  
instance Pointed' Id where
  pure = Id
  
  
instance Applicative' Id where
  Id f <*> Id x = Id (f x)
  
  
instance Monad' Id where
  join (Id (Id x)) = Id x
  
  


instance Functor' [] where
  fmap _ []       = []
  fmap f (x:xs)   = f x : fmap f xs
  
  
instance Pointed' [] where
  pure x = [x]
  
  
instance Semigroup' [a] where
  (<|>) = (++)


instance Monoid' [a] where
  empty = []
  
  
instance Applicative' [] where
  fs <*> xs = concat $ fmap (\f -> fmap f xs) fs


instance Monad' [] where
  join = concat


  
  
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
  
  
instance Semigroup' (Maybe a) where
  Just x  <|>  _  = Just x
  Nothing <|>  y  = y
  
  
instance Monoid' (Maybe a) where
  empty           = Nothing

{-
-- I'm not sure whether this instance makes any sense to me
instance Monoid' a => Monoid' (Maybe a) where
  empty                 = Nothing
  Nothing  <|> b        = b
  a        <|> Nothing  = a
  (Just a) <|> (Just b) = Just (a <|> b)
-}

instance Pointed' Maybe where
  pure = Just


instance Applicative' Maybe where
  Nothing  <*>    _     = Nothing
  _        <*> Nothing  = Nothing
  (Just f) <*> (Just x) = Just (f x)


instance Monad' Maybe where
  join (Just (Just x))  = Just x
  join      _           = Nothing
  
  
  
  
instance Semigroup' () where
  _ <|> _ = ()


instance Monoid' () where
  empty   = ()








