module Parse.Mt (

) where

import Classes
import Instances
import Datums
import Prelude     hiding (fmap, (>>=), (>>), fail, foldr, foldl)



newtype Reader a b 
    = Reader {
        getReader :: a -> b
    }

instance Functor' (Reader a) where
  -- :: (a -> b) -> Reader c a -> Reader c b
  fmap f (Reader g) = Reader (f . g)

instance Pointed' (Reader a) where
  -- :: b -> Reader a b
  pure x = Reader (const x)

instance Applicative' (Reader a) where
  -- :: Reader a (b -> c) -> Reader a b -> Reader a c
  Reader f <*> Reader g = Reader (\a -> f a (g a))

instance Monad' (Reader a) where
  -- :: Reader a (Reader a b) -> Reader a b
  join (Reader r) = Reader (\a -> getReader (r a) a)

instance Semigroup' b => Semigroup' (Reader a b) where
  Reader f <|> Reader g = Reader (\x -> f x <|> g x)

instance Monoid' b => Monoid' (Reader a b) where
  empty = Reader (const empty)



newtype Writer a b
    = Writer {
        getWriter :: (a, b)
    } 
  deriving (Show, Eq, Ord)

instance Functor' (Writer a) where
  fmap f = Writer . fmap f . getWriter

instance Monoid' a => Pointed' (Writer a) where
  pure = Writer . pure

instance Semigroup' a => Applicative' (Writer a) where
  f <*> x = Writer (getWriter f <*> getWriter x)
--  Writer (a1, f) <*> Writer (a2, x) = Writer (a1 <|> a2, f x)

instance Monoid' a => Monad' (Writer a) where
  join (Writer (a1, w)) = let Writer (a2, b) = w
                          in Writer (a1 <|> a2, b)

instance Copointed' (Writer a) where
  extract = extract . getWriter

instance Comonad' (Writer a) where
  duplicate (Writer (a, b)) = Writer (a, Writer (a, b))

instance (Semigroup' a, Semigroup' b) => Semigroup' (Writer a b) where
  Writer w1 <|> Writer w2   =  Writer (w1 <|> w2)

instance (Monoid' a, Monoid' b) => Monoid' (Writer a b) where
  empty = Writer empty

instance Foldable' (Writer a) where
  foldr f base = foldr f base . getWriter

instance Traversable' (Writer a) where
  commute (Writer (a, b)) = fmap (Writer . (,) a) b



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
  pure a = RW (Reader (\z -> Writer (z, a)))

instance Applicative' (RW z) where
  -- RW z (a -> b) -> RW z a -> RW z b
  -- R z (W z (a -> b)) -> R z (W z a) -> R z (W z b)
  -- (z -> W z (a -> b)) -> (z -> W z a) -> (z -> W z b)
  -- (z -> (z, a -> b)) -> (z -> (z, a)) -> (z -> (z, b))
  RW (Reader f) <*> RW (Reader x) = RW (Reader h)
    where
      h z1 = let Writer (z2, f') = f z1 
                 Writer (z3, x') = x z2
             in Writer(z3, f' x')

instance Monad' (RW z) where
  -- RW z (RW z a) -> RW z a
  -- R z (W z (R z (W z a))) -> R z (W z a)
  -- (z -> (z, z -> (z, a))) -> z -> (z, a)
  join (RW (Reader r1)) = RW (Reader h)
    where 
      h z1 = let Writer (z2, RW r2) = r1 z1
                 Writer (z3, a)     = getReader r2 z2
             in Writer (z3, a)



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
  pure x = WR (Writer (empty, Reader (const x)))

-- I have no idea what this instance does ... 
--   what's the point ???
instance Semigroup' z => Applicative' (WR z) where
  -- WR z (a -> b) -> WR z a -> WR z b
  -- W z (R z (a -> b)) -> W z (R z a) -> W z (R z b)
  -- (z, z -> a -> b) -> (z, z -> a) -> (z, z -> b)
  WR f <*> WR x = WR (Writer h)
    where
      Writer (z1, Reader f') = f
      Writer (z2, Reader x') = x
      h = (z1 <|> z2, Reader (\z3 -> f' z3 (x' z3)))

instance Monoid' z => Monad' (WR z) where
  -- WR z (WR z a)
  -- W z (R z (W z (R z a))) -> W z (R z a)
  -- (z, z -> (z, z -> a)) -> (z, z -> a)
  join (WR (Writer w)) = WR (Writer h)
    where
      (z1, Reader r1) = w
      WR (Writer (z2, r2)) = r1 z1
      h = (z1 <|> z2, r2)


