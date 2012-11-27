module IParse (

    ParserT

  , getOne
  , check
  , satisfy
  , literal
  
  , (<*)
  , (*>)
  , some
  , many
  
  , optional
  , sepBy1
  , sepBy0
  
  , end
  , not1
  , pnot
  , pnone
  , string

) where

import Classes
import Instances
import Prelude     hiding (fmap, (>>=), (>>), fail, foldr, foldl)



class ParserT m where
  getOne :: m t t
  
  check :: (a -> Bool) -> m t a -> m t a
  



satisfy :: ParserT m => (t -> Bool) -> m t t
satisfy p = check p getOne
  
  
literal :: (ParserT m, Eq t) => t -> m t t
literal tok = satisfy (== tok)
  
  
-- match both parsers in sequence, and return 
--   the value of the second parser
(*>) :: Applicative' f => f a -> f b -> f b
l *> r = fmap (flip const) l <*> r 


-- match both parsers in sequence, and return
--   the value of the first parser
(<*) :: Applicative' f => f a -> f b -> f a
l <* r = fmap const l <*> r


-- match parser 0 or more times
--   couldn't this also be accomplished with a fold?
many :: (Semigroup' (f [a]), Pointed' f, Applicative' f) => f a -> f [a]
many p = some p <|> pure []

        
-- match parser 1 or more times
some :: (Semigroup' (f [a]), Pointed' f, Applicative' f) => f a -> f [a]
some p = fmap (:) p <*> many p


optional :: (Semigroup' (f (Maybe a)), Pointed' f, Functor' f) =>
     f a -> f (Maybe a)
optional p = fmap Just p <|> pure Nothing

optionalM :: (Semigroup' (m s), Pointed' m) => m s -> s -> m s
optionalM p x = p <|> pure x


sepBy1 :: (Semigroup' (f ([a], [a1])), Pointed' f, Applicative' f) =>
     f a -> f a1 -> f ([a], [a1])
sepBy1 p s = fmap g p <*> (liftA2 f s (sepBy1 p s) <|> pure ([], []))
  where 
    f a (b, c) = (b, a:c)
    g d (e, f) = (d:e, f) -- why are we shadowing f here ???
    

sepBy0 :: (Semigroup' (f ([a], [a1])), Pointed' f, Applicative' f) =>
     f a -> f a1 -> f ([a], [a1])
sepBy0 p s = sepBy1 p s <|> pure ([], [])


end :: (ParserT m, Switch' (m t)) => m t ()
end = switch getOne


not1 :: (ParserT m, Switch' (m t), Applicative' (m t)) => m t a -> m t t
not1 p = switch p *> getOne


-- matches if next token is not x, consuming one token
pnot :: (Eq t, ParserT m) => t -> m t t
pnot x = satisfy (/= x)
-- how about:
--  pnot x = not1 (literal x)
--  pnot = not1 . literal


-- matches if next token not in xs, consuming one token
--   not sure if I like this one
pnone :: (Eq t, ParserT m) => [t] -> m t t
pnone xs = satisfy (\x -> not $ elem x xs)
-- how about:
--  pnone xs = not1 (pany $ map literal xs)
--  pnone = not1 . pany . map literal


-- matches all of the tokens in sequence
string :: (ParserT m, Eq t, Pointed' (m t), Applicative' (m t)) => [t] -> m t [t]
string = commute . map literal
