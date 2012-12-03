{-# LANGUAGE FunctionalDependencies #-}
module NonTrans (

) where

import Classes
import Datums
import Instances
import Prelude hiding ((>>=), (>>), fail, foldl, foldr, fmap)



class Parser m where
  getOne :: Monad' (m t) => m t t
  satisfy :: Monad' (m t) => (a -> Bool) -> m t a


instance Monad' (m t) => Monad' (Parser m) where
  join = undefined


class Monoid' m => IsZero m where
  isZero :: m -> Bool


class Fail m e | m -> e where
  fail :: e -> m a
  isFail :: m a -> Bool
  plus :: m a -> m a -> m a


class Error m e | m -> e where
  err :: e -> m a
  isErr :: m a -> Bool


class (Fail m e, Error m e) => Commit m e | m -> e where
  commit :: m a -> m a
  

newtype Parse1 t a
    = Parse1 {getParse1 :: [t] -> Maybe ([t], a)}

instance Functor' (Parse1 t) where
  fmap f (Parse1 g) = Parse1 h
    where h xs = fmap (fmap f) (g xs)

instance Pointed' (Parse1 t) where
  pure x = Parse1 (\xs -> Just (xs, x))

instance Applicative' (Parse1 t) where
  f <*> x = 
      f >>= \f' ->
      x >>= \x' ->
      pure (f' x')

instance Monad' (Parse1 t) where
  -- Parse1 t (Parse1 t a) -> Parse1 t a
  --     ([t] -> Maybe ([t], [t] -> Maybe ([t], a)) ) 
  --  -> [t] -> Maybe ([t], a)
  join (Parse1 f) = Parse1 h
    where 
      h xs1 = 
          f xs1 >>= \(xs2, p) -> 
          getParse1 p xs2

instance Parser Parse1 where
  getOne = Parse1 h
    where
      h [] = Nothing
      h (x:xs) = Just (xs, x)
  satisfy f = 
      getOne       >>= \x ->
      guard (f x)  >>
      pure x
