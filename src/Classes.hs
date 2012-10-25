module Classes (

) where

import Prelude hiding (fmap, (>>=), (>>), fail)


class Functor' f where
  fmap :: (a -> b) -> f a -> f b
  
  
class Pointed' f where
  pure :: a -> f a
  

{-  
class Comonad' m where
-}

  
class (Functor' f) => Applicative' f where
  (<*>) :: f (a -> b) -> f a -> f b
  
  
class (Applicative' m) => Monad' m where
  (>>=) :: m a -> (a -> m b) -> m b
  
  
  
  
  
-- ---------------
-- some instances

instance Functor' [] where
  fmap _ []       = []
  fmap f (x:xs)   = f x : fmap f xs
  
  
instance Pointed' [] where
  pure x = [x]
  
  
instance Applicative' [] where
  fs <*> xs = concat $ fmap (\f -> fmap f xs) fs

  
instance Monad' [] where
--  m a -> (a -> m b) -> m b
  q >>= f = concat $ fmap f q
  
  
instance Functor' IO where
  fmap f x = do { x' <- x; return (f x')}
  
  
instance Pointed' IO where
  pure = return
  
  
instance Applicative' IO where
  fs <*> xs = do {f <- fs; x <- xs; return (f x)}
  
  
instance Monad' IO where
  q >>= f = do {q' <- q; f q'}