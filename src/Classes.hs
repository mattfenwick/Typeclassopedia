module Classes (

    Functor'
  , fmap

  , Applicative'
  , (<*>)
  , liftA2

  , Pointed'
  , pure
  
  , Monad'
  , join
  , (>>=)
  , (>>)
  
  , Copointed'
  , extract
  
  , Comonad'
  , duplicate
  , extend

  , Semigroup'
  , (<|>)

  , Monoid'
  , empty
  , mconcat
  , guard
  
  , APlus'
  , (<+>)
  
  , AZero'
  , zero
  , guardA
  
  , AOr'
  , (<||>)
  
  , And'
  , (<&&>)
  
  , Switch'
  , switch

  , Foldable'
  , foldr
  , foldMap
  , fold
  , foldl
  
  , Traversable'
  , commute
  , traverse

) where

import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)
import qualified Prelude
-- import qualified Data.List




class Functor' f where
  fmap :: (a -> b) -> f a -> f b
  
  
class Functor' f => Applicative' f where
  (<*>) :: f (a -> b) -> f a -> f b

  
class Pointed' f where
  pure :: a -> f a
  

class (Applicative' m, Pointed' m) => Monad' m where
  join :: m (m a) -> m a
  

class Functor' f => Copointed' f where
  extract :: f a -> a

  
class Copointed' w => Comonad' w where
  duplicate :: w a -> w (w a)


class Semigroup' a where
  (<|>)  :: a -> a -> a


class Semigroup' a => Monoid' a where
  empty :: a


-- (a <+> b) <+> c = a <+> (b <+> c)
class APlus' f where
  (<+>) :: f a -> f a -> f a


--    a   <+>  zero  =  a
--  zero  <+>    b   =  b
class APlus' f => AZero' f where
  zero :: f a


-- see http://www.haskell.org/haskellwiki/MonadPlus_reform_proposal
-- pure a <||> x = pure a
class APlus' f => AOr' f where
  (<||>) :: f a -> f a -> f a


-- distributive: not sure how to express this, but here's a whack at it:
-- a <&&> (b ?? c)  =  (a <&&> b) ?? (a <&&> c)
class AZero' f => And' f where
  (<&&>) :: f a -> f a -> f a
  
  
class Switch' f where
  switch :: f a -> f ()
  
  
class Foldable' t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  
  
class (Functor' t, Foldable' t) => Traversable' t where
  -- law 1.  traverse Identity = Identity
  -- law 2.  traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
  --    (see Data.Functor.Compose)
  
  -- 'commute' instead of 'sequenceA': not false cognate with 'sequence'
  commute :: (Pointed' f, Applicative' f) => t (f a) -> f (t a)
  traverse :: (Pointed' f, Applicative' f) => (a -> f b) -> t a -> f (t b)

  commute = traverse id
  traverse f = commute . fmap f
  
  
-- -------------------------------
-- some more combinators


liftA2 :: Applicative' f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a1 a2 = fmap f a1 <*> a2


(>>=) :: Monad' m => m a -> (a -> m b) -> m b  
m >>= f = join (fmap f m)


(>>) :: Monad' m => m a -> m b -> m b
m >> f = m >>= const f


extend :: Comonad' w => (w a -> b) -> w a -> w b
extend f = fmap f . duplicate


mconcat :: Monoid' a => [a] -> a
mconcat = Prelude.foldr (<|>) empty


guard :: (Pointed' m, Monoid' (m ())) => Bool -> m ()
guard True   =  pure ()
guard False  =  empty


guardA :: (Pointed' m, AZero' m) => Bool -> m ()
guardA True   =  pure ()
guardA False  =  zero


foldl :: Foldable' t => (b -> a -> b) -> b -> t a -> b
foldl = error "haven't gotten up the courage to try this yet"


foldMap :: (Foldable' t, Monoid' m) => (a -> m) -> t a -> m
foldMap f = foldr (\e base -> f e <|> base) empty


fold :: (Foldable' t, Monoid' m) => t m -> m  
fold = foldr (<|>) empty
