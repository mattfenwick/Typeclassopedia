module Trans.Combs (

    check
  , satisfy
  , literal
  
  , (*>)
  , (<*)
  , many
  , some
  , optional
  , optionalM
  
  , sepBy0
  , sepBy1
  
  , end
  , not1
  , pnot
  , pnone
  , string

) where

import Classes
import Datums
import Instances
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))
import Trans.Parse
import Trans.MTrans


check :: (AZero' m, Monad' m) => (a -> Bool) -> m a -> m a
check f p =
    p             >>= \x ->
    guardA (f x)  >>
    pure x

satisfy :: (MonadParser a m, AZero' m) => (a -> Bool) -> m a
satisfy p = check p item

literal :: (Eq a, MonadParser a m, AZero' m) => a -> m a
literal tok = satisfy (== tok)

(*>) :: Applicative' f => f a -> f b -> f b
l *> r = fmap (flip const) l <*> r 

(<*) :: Applicative' f => f a -> f b -> f a
l <* r = fmap const l <*> r

many :: (Pointed' f, APlus' f, Applicative' f) => f a -> f [a]
many p = some p <+> pure []

some :: (Pointed' f, APlus' f, Applicative' f) => f a -> f [a]
some p = fmap (:) p <*> many p

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

end :: (Switch' f, MonadParser a f) => f ()
end = switch item

not1 :: (Switch' f, MonadParser b f) => f a -> f b
not1 p = switch p *> item

pnot :: (Eq a, MonadParser a m, AZero' m) => a -> m a
pnot x = satisfy (/= x)

pnone :: (Eq a, MonadParser a m, AZero' m) => [a] -> m a
pnone xs = satisfy (\x -> not $ elem x xs)

string :: (Eq a, MonadParser a f, AZero' f) => [a] -> f [a]
string = commute . map literal
