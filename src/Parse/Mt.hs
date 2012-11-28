module Parse.Mt (

) where

import Classes
import Instances
import Datums
import Prelude     hiding (fmap, (>>=), (>>), fail, foldr, foldl)



type Reader a b = a -> b



type Writer a b = (a, b)



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


