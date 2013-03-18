{-# LANGUAGE FlexibleContexts #-}
module Trans.Combs (

    item
  , check
  , satisfy
  , literal
  
  , end
  , not1
  , pnot
  , pnone
  , string

) where

import Classes
import Combinators
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))
import Trans.MTrans


item :: (AZero' m, MonadState [t] m) => m t
item = 
    get >>= \s -> case s of []     -> zero; 
                            (x:xs) -> put xs >> pure x

check :: (AZero' m, Monad' m) => (a -> Bool) -> m a -> m a
check f p =
    p             >>= \x ->
    guardA (f x)  >>
    pure x

satisfy :: (MonadState [a] m, AZero' m) => (a -> Bool) -> m a
satisfy p = check p item

literal :: (Eq a, MonadState [a] m, AZero' m) => a -> m a
literal tok = satisfy (== tok)

end :: (Switch' f, AZero' f, MonadState [a] f) => f ()
end = switch item

not1 :: (Switch' f, AZero' f, MonadState [b] f) => f a -> f b
not1 p = switch p *> item

pnot :: (Eq a, MonadState [a] m, AZero' m) => a -> m a
pnot x = satisfy (/= x)

pnone :: (Eq a, MonadState [a] m, AZero' m) => [a] -> m a
pnone xs = satisfy (\x -> not $ elem x xs)

string :: (Eq a, MonadState [a] f, AZero' f) => [a] -> f [a]
string = commute . map literal
