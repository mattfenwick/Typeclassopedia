module Thing (

    Thing(..)

) where

import Classes
import Instances () -- what does this do?
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)


data Thing a b c
    = Error a
    | Fail  b
    | Ok    c
  deriving (Show, Eq, Ord)


instance Functor' (Thing a b) where
  fmap f (Ok x)     =  Ok (f x)
  fmap _ (Fail y)   =  Fail y
  fmap _ (Error z)  =  Error z

instance Pointed' (Thing a b) where
  pure = Ok

instance Applicative' (Thing a b) where
  Ok f     <*>   x    =  fmap f x
  Fail y   <*>   _    =  Fail y
  Error z  <*>   _    =  Error z

instance Monad' (Thing a b) where
  join (Ok (Ok x))  =  Ok x
  join (Ok q)       =  q
  join (Fail y)     =  Fail y
  join (Error z)    =  Error z

instance Semigroup' (Thing a b c) where
  Ok x      <|>  _   =  Ok x
  Error z   <|>  _   =  Error z
  Fail _    <|>  r   =  r

instance Foldable' (Thing a b) where
  foldr f base (Ok x)  =  f x base
  foldr _ base   _     =  base

instance Traversable' (Thing a b) where
  commute (Ok    x)  =  fmap Ok x
  commute (Fail  y)  =  pure (Fail y)
  commute (Error z)  =  pure (Error z)
