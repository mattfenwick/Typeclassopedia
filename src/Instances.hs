module Instances (

) where

import Datums
import Classes
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)




instance Semigroup' () where
  _ <|> _ = ()

instance Monoid' () where
  empty   = ()




instance Functor' ((->) z) where
  -- (a -> b) -> (z -> a) -> (z -> b)
  fmap f g = f . g

instance Applicative' ((->) z) where
  -- (z -> (a -> b)) -> (z -> a) -> (z -> b)
  f <*> g = \x -> f x (g x)

instance Pointed' ((->) z) where
  -- a -> (z -> a)
  pure = const

instance Monad' ((->) z) where
  -- (z -> (z -> a)) -> (z -> a)
  join f = \x -> (f x) x
  --   I don't understand how this works

instance Semigroup' a => Semigroup' ((->) z a) where
  -- (z -> a) -> (z -> a) -> (z -> a)
  f <|> g = \x -> f x <|> g x

instance Monoid' a => Monoid' ((->) z a) where
  -- z -> a
  empty = const empty

instance Foldable' ((->) z) where
  -- (a -> b -> b) -> b -> (z -> a) -> b
  --  foldr f base g = f (g empty) base -- if we allow a monoid constraint on z
  --  foldr _ base _ = base
  foldr _ = const




instance Functor' (Reader z) where
  -- (a -> b) -> Reader z a -> Reader z b
  -- (a -> b) -> (z -> a) -> (z -> b)
  fmap f (Reader g) = Reader (f . g)

instance Applicative' (Reader z) where
  -- Reader z (a -> b) -> Reader z a -> Reader z b
  -- (z -> a -> b) -> (z -> a) -> (z -> b)
  Reader f <*> Reader g = Reader (\x -> f x (g x))

instance Pointed' (Reader z) where
  -- a -> Reader z a
  -- a -> (z -> a)
  pure = Reader . const

instance Monad' (Reader z) where
  -- Reader z (Reader z a) -> Reader z a
  -- (z -> z -> a) -> (z -> a)
  join (Reader f) = Reader (\x -> getReader (f x) x)




instance Functor' ((,) a) where
  fmap f (x, y) = (x, f y)

instance (Semigroup' z) => Applicative' ((,) z) where
  (x1, f) <*> (x2, y) = (x1 <|> x2, f y)

instance (Monoid' z) => Pointed' ((,) z) where
  pure x = (empty, x)

instance (Monoid' z) => Monad' ((,) z) where
  join (x1, (x2, a)) = (x1 <|> x2, a)
  
instance Copointed' ((,) z) where
  extract = snd
  
instance Comonad' ((,) z) where
  duplicate (x, y) = (x, (x, y))

instance (Semigroup' a, Semigroup' b) => Semigroup' (a, b) where
  (x1, y1) <|> (x2, y2) = (x1 <|> x2, y1 <|> y2)

instance (Monoid' a, Monoid' b) => Monoid' (a, b) where
  empty = (empty, empty)
  
instance Foldable' ((,) z) where
  foldr f base (_, x) = f x base
  
instance Traversable' ((,) z) where
  commute      (z, y)  =  fmap ((,) z) y
  traverse  f  (x, y)  =  pure (,) <*> pure x <*> f y




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

instance APlus' [] where
  (<+>)  =  (++)

instance AZero' [] where
  zero    =  []

instance AOr' [] where
  []  <||>  r  =  r
  l   <||>  _  =  l

instance Switch' [] where
  switch []     = [()]
  switch (_:_)  = []

instance Foldable' [] where
  foldr _ base []       = base
  foldr f base (x:xs)   = f x (foldr f base xs)
  
instance Traversable' [] where
  commute    []      =  pure []
  commute    (f:fs)  =  pure (:) <*> f   <*> commute fs
  traverse _ []      =  pure []
  traverse f (x:xs)  =  pure (:) <*> f x <*> traverse f xs




instance Functor' Seq where
  fmap f (End x)         = End (f x)
  fmap f (Cons x rest)   = Cons (f x) (fmap f rest)
  
instance Applicative' Seq where
  fs <*> xs = join $ fmap (\f -> fmap f xs) fs
  
instance Pointed' Seq where
  pure = End
  
instance Monad' Seq where
  join (Cons (End x) rest)          = Cons x (join rest)
  join (Cons (Cons x rest1) rest2)  = Cons x (join (Cons rest1 rest2))
  join (End xs)                     = xs
  
instance Copointed' Seq where
  extract (End x)      = x
  extract (Cons x _)   = x

instance Comonad' Seq where
  duplicate xs = fmap (const xs) xs

instance Semigroup' (Seq a) where
  (End x)   <|> ys = Cons x ys
  Cons x xs <|> ys = Cons x (xs <|> ys)

instance Foldable' Seq where
  foldr f base (End x)     = f x base
  foldr f base (Cons x xs) = f x (foldr f base xs)

instance Traversable' Seq where
  commute    (End x)      =  fmap pure x
  commute    (Cons x xs)  =  fmap Cons x <*> commute xs
  traverse f (End x)      =  pure End <*> f x
  traverse f (Cons x xs)  =  pure Cons <*> f x <*> traverse f xs

  
  
  
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

instance APlus' Maybe where
  Just x   <+>  _  =  Just x
  Nothing  <+>  y  =  y

instance AZero' Maybe where
  zero = Nothing

instance AOr' Maybe where
  Nothing  <||>   r   =  r
  l        <||>   _   =  l

instance Switch' Maybe where
  switch Nothing   = Just ()
  switch (Just _)  = Nothing
  
instance Foldable' Maybe where
  foldr _ base Nothing = base
  foldr f base (Just x) = f x base
  
instance Traversable' Maybe where
  commute      Nothing   =  pure Nothing
  commute      (Just x)  =  fmap Just x
  traverse  _  Nothing   =  pure Nothing
  traverse  f  (Just x)  =  pure Just <*> f x




instance Functor' (Either a) where
  fmap _ (Left x)   =  Left x
  fmap f (Right r)  =  Right (f r)

instance Applicative' (Either a) where
  Left x   <*>   _   =  Left x
  Right f  <*>   x   =  fmap f x

instance Pointed' (Either a) where
  pure = Right

instance Monad' (Either a) where
  join (Right (Right z))  =  Right z
  join (Right (Left z))   =  Left z
  join (Left y)           =  Left y

-- left-bias both success and failure
instance Semigroup' (Either a b) where
  Right x  <|>     _     =  Right x
  _        <|>  Right z  =  Right z 
  Left y   <|>     _     =  Left y

instance Monoid' a => Monoid' (Either a b) where
  empty = Left empty

instance APlus' (Either a) where
  (<+>) = (<|>)

instance Monoid' a => AZero' (Either a) where
  zero = Left empty

instance AOr' (Either a) where
  (<||>) = (<|>)

instance Foldable' (Either a) where
  foldr _ base (Left _) = base
  foldr f base (Right x) = f x base
  
instance Traversable' (Either m) where
  commute      (Left x)   =  pure (Left x)
  commute      (Right r)  =  fmap Right r
  traverse  _  (Left x)   =  pure (Left x)
  traverse  f  (Right r)  =  pure Right <*> f r




instance Functor' Id where
  fmap f (Id x) = Id (f x)
  
instance Applicative' Id where
  Id f <*> Id x = Id (f x)
  
instance Pointed' Id where
  pure = Id
  
instance Monad' Id where
  join (Id (Id x)) = Id x
  
instance Copointed' Id where
  extract (Id x) = x
  
instance Comonad' Id where
  duplicate (Id x) = Id (Id x)

instance Semigroup' a => Semigroup' (Id a) where
  Id x  <|>  Id y  =  Id  (x <|> y)

instance Monoid' a => Monoid' (Id a) where
  empty = Id empty
  
instance Foldable' Id where
  foldr f base (Id x) = f x base
  
instance Traversable' Id where
  commute      (Id x)  =  fmap Id x
  traverse  f  (Id x)  =  pure Id   <*>  f x



instance Functor' (State s) where
  -- :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State (\s -> fmap f (g s))

instance Applicative' (State s) where
  -- :: State s (a -> b) -> State s a -> State s b
  State f <*> State x = State (\s -> let (s', f') = f s
                                         (s'', x') = x s'
                                     in (s'', f' x'))

instance Pointed' (State s) where
  pure x = State (\s -> (s, x))

instance Monad' (State s) where
  -- :: State s (State s a) -> State s a
  -- :: s -> (s, s -> (s, a)) -> s -> (s, a)
  join (State s1) = State (\s -> let (s', s2) = s1 s
                                 in getState s2 s')




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
  fmap f (Leaf x)      = Leaf (f x)
  fmap f (Node l r)    = Node (fmap f l) (fmap f r)
  
instance Applicative' BinTree where
  (Leaf f)   <*> (Leaf x)     = Leaf (f x)
  (Leaf f)   <*> (Node y z)   = Node (Leaf f <*> y) (Leaf f <*> z)
  (Node g h) <*> (Leaf x)     = Node (g <*> Leaf x) (h <*> Leaf x)
  (Node g h) <*> (Node y z)   = Node (g <*> y) (h <*> z)
  
instance Pointed' BinTree where
  pure = Leaf

instance Monad' BinTree where
  join (Leaf y)     = y
  join (Node l r)   = Node (join l) (join r)
  
-- this is left biased
instance Copointed' BinTree where
  extract (Leaf x)     = x
  extract (Node l _)   = extract l
  
instance Comonad' BinTree where
  duplicate bt   = fmap (const bt) bt

-- take the bigger subtree
--   if tied, take left subtree
instance Semigroup' (BinTree a) where
  Node l l'  <|>  Node r r'   =  Node (l <|> r) (l' <|> r')
  Leaf _     <|>  Node r r'   =  Node r r'
  x          <|>  _           =  x
  
instance Foldable' BinTree where
  foldr f base (Leaf x)     = f x base
  foldr f base (Node l r)   = let base' = foldr f base r
                              in foldr f base' l

instance Traversable' BinTree where
  commute      (Leaf x)    =  fmap Leaf x 
  commute      (Node l r)  =  fmap Node (commute l) <*> (commute r)
  traverse  f  (Leaf x)    =  pure Leaf <*> f x
  traverse  f  (Node l r)  =  pure Node <*> traverse f l <*> traverse f r




instance Functor' Tree where
  fmap f (Tree x bs) = Tree (f x) (fmap (fmap f) bs)

instance Applicative' Tree where
-- 1. treat lists as a zipList:
--  Tree f fs  <*>  Tree x xs = Tree (f x) (zipWith (<*>) fs xs)
-- 2. treat lists as nondeterministic
-- :: Tree (a -> b) -> Tree a -> Tree b
  Tree f fs  <*>  Tree x xs = Tree (f x) (liftA2 (<*>) fs xs)
-- 3. treat entire trees as non-deterministic (like in standard Tree Applicative)

instance Pointed' Tree where
  pure x = Tree x []

instance Monad' Tree where
  join (Tree (Tree x xs) ys) = Tree x (xs ++ map join ys)

instance Copointed' Tree where
  extract (Tree x _) = x

instance Comonad' Tree where
  duplicate t = fmap (const t) t

-- it seems like this would be more useful
--   if the two root elements were semigroup-combined
--   instead of just ignoring the left one
-- this would also allow a monoid instance
instance Semigroup' (Tree a) where
  Tree x xs  <|>  Tree x' xs'  =  Tree x (liftA2 (<|>) xs xs')

instance Foldable' Tree where
  foldr f base (Tree x xs) = f x (foldList (\t b -> foldTree f b t) base xs)
    where
      foldList :: (Tree a -> b -> b) -> b -> [Tree a] -> b
      foldList = foldr
      foldTree :: (a -> b -> b) -> b -> Tree a -> b
      foldTree = foldr

instance Traversable' Tree where
  commute      (Tree x bs)  =  fmap Tree x <*> (traverse commute bs)
  traverse  f  (Tree x bs)  =  pure Tree <*> f x <*> traverse (traverse f) bs
  -- wow ... traverse (traverse f) bs ... not sure what that means!




instance Functor' MyTree where
  fmap _ Empty            = Empty
  fmap f (Branch x l r)   = Branch (f x) (fmap f l) (fmap f r)

instance Applicative' MyTree where
  Empty         <*>  _                =  Empty
  _             <*>  Empty            =  Empty
  Branch f l r  <*>  Branch x l' r'   =  Branch (f x) (l <*> l') (r <*> r')

instance Pointed' MyTree where
  pure x = Branch x Empty Empty

-- take the bigger subtree
--   if tied, take left subtree
instance Semigroup' (MyTree a) where
  x             <|>  Empty             = x
  Empty         <|>  y                 = y
  Branch x l r  <|>  Branch x' l' r'   = Branch x (l <|> l') (r <|> r')

instance Monoid' (MyTree a) where
  empty = Empty

instance Switch' MyTree where
  switch Empty   = pure ()
  switch _       = Empty

instance Foldable' MyTree where
  foldr _ base Empty          = base
  foldr f base (Branch x l r) = let base' = foldr f base r
                                in let base'' = f x base'
                                in foldr f base'' l

instance Traversable' MyTree where
  commute      Empty           =  pure Empty
  commute      (Branch x l r)  =  fmap Branch x <*> commute l <*> commute r
  traverse  _  Empty           =  pure Empty
  traverse  f  (Branch x l r)  =  pure Branch <*> f x 
                                              <*> traverse f l 
                                              <*> traverse f r



instance (Functor' f, Functor' g) => Functor' (Compose f g) where
  fmap f = Compose . fmap (fmap f) . getCompose

instance (Pointed' f, Pointed' g) => Pointed' (Compose f g) where
  pure = Compose . pure . pure

instance (Pointed' f, Applicative' f, Applicative' g) => Applicative' (Compose f g) where
  f <*> x = Compose (pure (<*>) <*> getCompose f <*> getCompose x)

instance Foldable' (Compose f g) where
  foldr = error "oops!  too lazy"

instance (Traversable' f, Traversable' g) => Traversable' (Compose f g) where
  traverse f x = pure Compose <*> traverse (traverse f) (getCompose x)
