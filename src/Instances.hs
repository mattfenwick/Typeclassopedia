{-# LANGUAGE FlexibleInstances #-} -- for the Semigroup/Monoid instances of (a -> a)

module Instances (

) where

import Datums
import Classes
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)




instance Semigroup' () where
  _ <|> _ = ()

instance Monoid' () where
  empty   = ()




instance Functor' ((->) a) where
  -- (b -> c) -> (a -> b) -> (a -> c)
  fmap f g = f . g

instance Applicative' ((->) a) where
  -- (a -> (b -> c)) -> (a -> b) -> (a -> c)
  f <*> g = \x -> f x (g x)

instance Pointed' ((->) a) where
  pure = const

instance Monad' ((->) a) where
  -- (a -> (a -> b)) -> (a -> b)
  --   I don't understand how this works
  join f = \x -> (f x) x

-- apparently requires FlexibleInstances
instance Semigroup' ((->) a a) where
  (<|>) = (.)

instance Monoid' ((->) a a) where
  empty = id




instance Functor' ((,) a) where
  fmap f (x, y) = (x, f y)

instance (Semigroup' z) => Applicative' ((,) z) where
  (x1, f) <*> (x2, y) = (x1 <|> x2, f y) -- fmap f (x1 <|> x2, y)

instance (Monoid' z) => Pointed' ((,) z) where
  pure x = (empty, x)
  
instance Copointed' ((,) z) where
  extract = snd

instance (Monoid' z) => Monad' ((,) z) where
  join (x1, (x2, a)) = (x1 <|> x2, a)
  
instance Comonad' ((,) z) where
  duplicate (x, y) = (x, (x, y))

instance (Semigroup' a, Semigroup' b) => Semigroup' (a, b) where
  (x1, y1) <|> (x2, y2) = (x1 <|> x2, y1 <|> y2)

instance (Monoid' a, Monoid' b) => Monoid' (a, b) where
  empty = (empty, empty)
  
  


instance Functor' [] where
  fmap _ []       = []
  fmap f (x:xs)   = f x : fmap f xs
  
instance Applicative' [] where
  fs <*> xs = concat $ fmap (\f -> fmap f xs) fs

instance Pointed' [] where
  pure x = [x]
  
instance Monad' [] where
  join = concat

instance Semigroup' [a] where
  (<|>) = (++)

instance Monoid' [a] where
  empty = []
  
instance Switch' [] where
  switch []     = [()]
  switch (_:_)  = []  
  
instance Foldable' [] where
  foldr _ base []       = base
  foldr f base (x:xs)   = f x (foldr f base xs)
  
instance Traversable' [] where
  commute []       = pure []
  commute (f:fs)   = fmap (:) f <*> commute fs 
  
  
  
instance Functor' Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)
  
instance Applicative' Maybe where
  Nothing <*> _ = Nothing
  Just f  <*> x = fmap f x

instance Pointed' Maybe where
  pure = Just

instance Monad' Maybe where
  join (Just (Just x))  = Just x
  join      _           = Nothing

instance Semigroup' (Maybe a) where
  Just x  <|>  _  = Just x
  Nothing <|>  y  = y
 
instance Monoid' (Maybe a) where
  empty           = Nothing
  
instance Switch' Maybe where
  switch Nothing   = Just ()
  switch (Just _)  = Nothing
  
instance Foldable' Maybe where
  foldr _ base Nothing = base
  foldr f base (Just x) = f x base
  
instance Traversable' Maybe where
  commute Nothing  = pure Nothing
  commute (Just x) = fmap Just x




instance Functor' (Either a) where
  fmap _ (Left x)  = Left x
  fmap f (Right r) = Right (f r)

instance Applicative' (Either a) where
  Left x  <*> _ = Left x
  Right f <*> x = fmap f x

instance Pointed' (Either a) where
  pure = Right

instance Monad' (Either a) where
  join (Right (Right z)) = Right z
  join (Right (Left z))  = Left z
  join (Left y)          = Left y

instance Semigroup' (Either a b) where
  Right x <|>    _    = Right x
  _       <|> Right y = Right y
  z       <|>    _    = z
  
instance Foldable' (Either a) where
  foldr _ base (Left _) = base
  foldr f base (Right x) = f x base
  
instance Traversable' (Either m) where
  commute (Left y)  = pure (Left y)
  commute (Right x) = fmap Right x




instance Functor' Id where
  fmap f (Id x) = Id (f x)
  
instance Applicative' Id where
  Id f <*> Id x = Id (f x)
  
instance Pointed' Id where
  pure = Id
  
instance Copointed' Id where
  extract (Id x) = x
  
instance Monad' Id where
  join (Id (Id x)) = Id x
  
instance Comonad' Id where
  duplicate (Id x) = Id (Id x)
  
instance Foldable' Id where
  foldr f base (Id x) = f x base
  
instance Traversable' Id where
  commute (Id x) = fmap Id x




instance Functor' IO where
  fmap f x = do { x' <- x; return (f x')}
  
instance Applicative' IO where
  fs <*> xs = do {f <- fs; x <- xs; return (f x)}
  
instance Pointed' IO where
  pure = return
  
instance Monad' IO where
  join m = do {x <- m; y <- x; return y}
  
  


instance (Num a) => Semigroup' (Product a) where
  Product x <|> Product y = Product (x * y)

instance (Num a) => Monoid' (Product a) where
  empty = Product 1




instance Functor' BinTree where
  fmap f (Leaf x)          = Leaf (f x)
  fmap f (Node left right) = Node (fmap f left) (fmap f right)
  
instance Pointed' BinTree where
  pure = Leaf
  
instance Foldable' BinTree where
  foldr f base (Leaf x) = f x base
  foldr f base (Node left right) = let base' = foldr f base right
                                   in foldr f base' left




instance Functor' Tree where
  fmap f (Tree x bs) = Tree (f x) (fmap (fmap f) bs)
  
instance Pointed' Tree where
  pure x = Tree x []


