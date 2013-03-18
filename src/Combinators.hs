module Combinators (

    (*>)
  , (<*)
  , many0
  , many1
  
  , optional
  , optionalM
  
  , sepBy0
  , sepBy1
  
) where

import Classes
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))



(*>) :: Applicative' f => f a -> f b -> f b
l *> r = fmap (flip const) l <*> r 

(<*) :: Applicative' f => f a -> f b -> f a
l <* r = fmap const l <*> r

many0 :: (Pointed' f, APlus' f, Applicative' f) => f a -> f [a]
many0 p = many1 p <+> pure []

many1 :: (Pointed' f, APlus' f, Applicative' f) => f a -> f [a]
many1 p = fmap (:) p <*> many0 p

optional :: (Pointed' f1, AZero' f1, Pointed' f, Functor' f, APlus' f) =>
     f a -> f (f1 a)
optional p = fmap pure p  <+>  pure zero

optionalM :: (Pointed' f, APlus' f) => a -> f a -> f a
optionalM x p = p <+> pure x

sepBy1 :: (Pointed' f, Applicative' f, APlus' f) => f a -> f a1 -> f ([a], [a1])
sepBy1 p s = fmap g p <*> (liftA2 f s (sepBy1 p s) <+> pure ([], []))
  where 
    f a (b, c) = (b, a:c)
    g x (y, z) = (x:y, z)
    
sepBy0 :: (Pointed' f, Applicative' f, APlus' f) => f a -> f a1 -> f ([a], [a1])
sepBy0 p s = sepBy1 p s <+> pure ([], [])

