{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies, UndecidableInstances, FlexibleInstances #-}
module Trans.Parse (

) where

import Classes
import Datums
import Instances
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))
import Trans.MTrans


type Parser t m a = StateT [t] m a

getT = undefined
putT = undefined

getOne :: (Monad' m, Monoid' (m ([t], t))) => Parser t m t
getOne = 
    getT >>= \xs -> case xs of 
                    (y:ys)  ->  putT ys >> pure y;
                    []      ->  empty;

class (Monad' m) => MonadParser t m | m -> t where
  item :: m t

instance (Monoid' (m ([t], t)), Monad' m) => MonadParser t (StateT [t] m) where
  -- StateT [t] m t
  item = getOne

-- newtype CounterParser -- <=== counts newlines and spaces ... maybe just
-- has to wrap/unwrap the StateT instances for Functor, Pointed, etc.?