{-# LANGUAGE NoMonomorphismRestriction #-}

module Parse.Mt (

    join2
  , bind2
  
  , RW(..)
  , WR(..)
  , RW1(..)
  , WR1(..)

) where

import Classes
import Instances
import Datums
import Prelude     hiding (fmap, (>>=), (>>), fail, foldr, foldl)



type Reader a b = a -> b



type Writer a b = (a, b)


join2 :: (Monad' m, Traversable' n, Monad' n) => m (n (m (n a))) -> m (n a)
join2 = 
    fmap join      .
    join           .
    fmap commute
    -- 4. m (n a)
    -- 3. m (n (n a))
    -- 2. m (m (n (n a)))
    -- 1. m (n (m (n a)))


-- bind2 :: m a -> (a -> m b) -> m b
bind2 :: (Monad' m, Traversable' n, Monad' n) => m (n a) -> (a -> m (n b)) -> m (n b)
bind2 m f = join2 (fmap (fmap f) m)



newtype RW a b 
    = RW {
        getRW :: Reader a (Writer a b)
    --  or    :: Reader a (a, b)
    --  or    :: a -> (a, b)
    }

instance Functor' (RW z) where
  -- (a -> b) -> RW z a -> RW z b
  -- (a -> b) -> R z (W z a) -> R z (W z b)
  fmap f = RW . fmap (fmap f) . getRW

instance Pointed' (RW z) where
  -- a -> RW z a
  -- a -> R z (W z a)
  -- a -> (z -> (z, a))
  pure a = RW (\z -> (z, a))

instance Applicative' (RW z) where
  -- RW z (a -> b) -> RW z a -> RW z b
  -- R z (W z (a -> b)) -> R z (W z a) -> R z (W z b)
  -- (z -> W z (a -> b)) -> (z -> W z a) -> (z -> W z b)
  -- (z -> (z, a -> b)) -> (z -> (z, a)) -> (z -> (z, b))
  RW f <*> RW x = RW h
    where
      h z1 = let (z2, f') = f z1 
                 (z3, x') = x z2
             in (z3, f' x')

instance Monad' (RW z) where
  -- RW z (RW z a) -> RW z a
  -- R z (W z (R z (W z a))) -> R z (W z a)
  -- (z -> (z, z -> (z, a))) -> z -> (z, a)
  join (RW r1) = RW h
    where 
      h z1 = let (z2, RW r2) = r1 z1
                 (z3, a)     = r2 z2
             in (z3, a)

-- instance Semigroup' (RW z a) where
  -- RW z a -> RW z a -> RW z a
  -- (z -> (z, a)) -> (z, (z, a)) -> (z, (z, a))

-- instance Monoid' (RW z a) where
  -- RW z a
  -- (z -> (z, a))


-- derivation:  ???? so which is the top (Reader) and which is the bottom (Writer) ?
--            (a, b)
--      Maybe (a, b)
-- a -> Maybe (a, b)
newtype RMW z a
    = RMW {
        getRMW :: Reader z (Maybe (Writer z a))
    -- or      :: z -> Maybe (z, a)
    }

instance Functor' (RMW z) where
  -- (a -> b) -> RMW z a -> RMW z b
  -- (a -> b) -> (z -> Maybe (z, a)) -> (z -> Maybe (z, b))
  fmap f = RMW . fmap (fmap (fmap f)) . getRMW

instance Pointed' (RMW z) where
  -- a -> RMW z a
  -- a -> (z -> Maybe (z, a))
  pure a = RMW (\z -> Just (z, a))

instance Applicative' (RMW z) where
  -- RMW z (a -> b) -> RMW z a -> RMW z b
  -- (z -> Maybe (z, a -> b)) -> (z -> Maybe (z, a)) -> (z -> Maybe (z, b))
  RMW f <*> RMW x = RMW h
    where
      h z1 = 
          f z1 >>= \(z2, f') ->
          x z2 >>= \(z3, x') ->
          Just (z3, f' x')

instance Monad' (RMW z) where
  -- RMW z (RMW z a) -> RMW z a
  -- (z -> Maybe (z, z -> Maybe (z, a))) -> z -> Maybe (z, a)
  join (RMW f1) = RMW h
    where
      h z1 = 
          f1 z1 >>= \(z2, RMW f2) ->
          f2 z2



newtype RW1 y z a
    = RW1 {
        getRW1 :: Reader y (Writer z a)
    -- or      :: Reader y (z, a)
    -- or      :: y -> (z, a)
    }

instance Functor' (RW1 y z) where
  -- (a -> b) -> RW1 y z a     -> RW1 y z b
  -- (a -> b) -> (y -> (z, a)) -> (y -> (z, b))
  fmap f = RW1 . fmap (fmap f) . getRW1

instance Monoid' z => Pointed' (RW1 y z) where
  -- a -> RW1 y z a
  -- a -> (y -> (z, a))
  pure = RW1 . const . ((,) empty)

instance Semigroup' z => Applicative' (RW1 y z) where
  -- RW1 y z (a -> b)   -> RW1 y z a     -> RW1 y z b
  -- (y -> (z, a -> b)) -> (y -> (z, a)) -> (y -> (z, b))
  RW1 f <*> RW1 x = RW1 h
    where
      h y = let (z1, f') = f y
                (z2, x') = x y
            in (z1 <|> z2, f' x')

instance Monoid' z => Monad' (RW1 y z) where
  -- RW1 y z (RW1 y z a) -> RW1 y z a
  -- R y (W z (R y (W z a))) -> R y (W z a)
  -- (y -> (z, y -> (z, a))) -> y -> (z, a)
  join = RW1 . join2 . getRW1 . fmap getRW1
--
-- 'old-school' implementation:
--  join (RW1 f1) = RW1 h
--    where
--      h y = let (z1, f2) = f1 y
--                (z2, x) = getRW1 f2 y
--            in (z1 <|> z2, x)
            


newtype WR z a 
    = WR {
        getWR :: Writer z (Reader z a)
    -- or     :: Writer z (z -> a)
    -- or     :: (z, z -> a)
    }

instance Functor' (WR z) where
  -- (a -> b) -> WR z a -> WR z b
  -- (a -> b) -> W z (R z a) -> W z (R z b)
  -- (a -> b) -> (z, z -> a) -> (z, z -> b)
  fmap f = WR . fmap (fmap f) . getWR

instance Monoid' z => Pointed' (WR z) where
  -- a -> WR z a
  -- a -> W z (R z a)
  -- a -> (z, z -> a)
  pure x = WR (empty, const x)

-- I have no idea what this instance does ... 
--   what's the point ???
instance Semigroup' z => Applicative' (WR z) where
  -- WR z (a -> b) -> WR z a -> WR z b
  -- W z (R z (a -> b)) -> W z (R z a) -> W z (R z b)
  -- (z, z -> a -> b) -> (z, z -> a) -> (z, z -> b)
  WR f <*> WR x = WR h
    where
      (z1, f') = f
      (z2, x') = x
      h = (z1 <|> z2, (\z3 -> f' z3 (x' z3)))

instance Monoid' z => Monad' (WR z) where
  -- WR z (WR z a)
  -- W z (R z (W z (R z a))) -> W z (R z a)
  -- (z, z -> (z, z -> a)) -> (z, z -> a)
  join (WR w) = WR h
    where
      (z1, r1) = w
      WR (z2, r2) = r1 z1
      h = (z1 <|> z2, r2)



newtype WR1 y z a
    = WR1 {
        getWR1 :: Writer y (Reader z a)
    -- or      :: (y, z -> a)
    }

instance Functor' (WR1 y z) where
  -- (a -> b) -> WR1 y z a -> WR1 y z b
  -- (a -> b) -> (y, z -> a) -> (y, z -> b)
  fmap f = WR1 . fmap (fmap f) . getWR1

instance Monoid' y => Pointed' (WR1 y z) where
  -- a -> WR1 y z a
  -- a -> (y, z -> a)
  pure x = WR1 (empty, const x)

instance Semigroup' y => Applicative' (WR1 y z) where
  -- WR1 y z (a -> b) -> WR1 y z a -> WR1 y z b
  -- (y, z -> (a -> b)) -> (y, z -> a) -> (y, z -> b)
  WR1 f <*> WR1 x = WR1 h
    where
      h = let (y1, f') = f
              (y2, x') = x
          in (y1 <|> y2, \z -> f' z (x' z))
{-
instance Monoid' y => Monad' (WR1 y z) where
  -- WR1 y z (WR1 y z a) -> WR1 y z a
  -- (y, z -> (y, z -> a)) -> (y, z -> a)
  join (WR1 m) = WR1 h
    where
      h = let (y1, f1) = m
              f3 z = let (y2, f2) = f1 z
                     in (y1 <|> y2, f2)
              (y3, f4) = f3 ??
          in (y3, \z -> 
    WR1 (y3, f3)
    where
      (y1, f1) = m
      f3 z = let (y2, f2) = f1 z
             in f2
      y3 = l
    where
      -}
{-
  bind :: WR1 y z a -> (a -> WR1 y z b) -> WR1 y z b
  bind :: (y, z -> a) -> (a -> (y, z -> b)) -> (y, z -> b)
  bind WR1 (y1, f1) g = WR1 h
    where
      h = (y3, f4)
        where
          
      f3 :: z -> (y, z -> b)
      f3 z = let a = f1 z
                 (y2, f2) = g a
                 in (y2, f2)
  bind WR1 m g = WR1 h
    where
      h = do $
          f <- m
          
-}

