module MParse (

    Parser(..)
  
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
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)



newtype Parser t a = Parser {
        getParser :: ( [t] -> Maybe ([t], a) )
    }




instance Functor' (Parser s) where
  -- one 'fmap' for the Maybe, one for the ((,) [t])
  fmap f (Parser g) = Parser (fmap (fmap f) . g)

instance Applicative' (Parser s) where
  Parser f <*> Parser x = Parser h
    where
      h xs = f xs >>= \(ys, f') -> 
        x ys >>= \(zs, x') ->
        Just (zs, f' x')
        
instance Pointed' (Parser s) where
  pure a = Parser (\xs -> Just (xs, a))
  
instance Monad' (Parser s) where
  join (Parser f) = Parser h
    where
      h xs = f xs >>= \(o, Parser g) -> g o  
  
instance Semigroup' (Parser s a) where
  Parser f <|> Parser g = Parser (\xs -> f xs <|> g xs)
  
instance Monoid' (Parser s a) where
  empty = Parser (const Nothing)
  
instance Switch' (Parser s) where
  switch (Parser f) = Parser h
    where h xs = fmap (const (xs, ())) $ switch (f xs)


-- -------------------------

-- succeeds, consuming one 'token', as
--   long as input is not empty
getOne :: Parser s s
getOne = Parser (\xs -> case xs of 
                        (y:ys) -> pure (ys, y);
                        _      -> empty)
  
  
check :: (a -> Bool) -> Parser s a -> Parser s a
check f p = p >>= \x -> 
  guard (f x) >> 
  pure x


satisfy :: (a -> Bool) -> Parser a a
satisfy p = check p getOne
  
  
literal :: Eq a => a -> Parser a a
literal tok = satisfy (== tok)
  
  
-- match both parsers in sequence, and return 
--   the value of the second parser
(*>) :: Parser t a -> Parser t b -> Parser t b
l *> r = fmap (flip const) l <*> r 


-- match both parsers in sequence, and return
--   the value of the first parser
(<*) :: Parser t a -> Parser t b -> Parser t a
l <* r = fmap const l <*> r


-- match parser 0 or more times
--   couldn't this also be accomplished with a fold?
many :: Parser t a -> Parser t [a]
many p = some p <|> pure []

        
-- match parser 1 or more times
some :: Parser t a -> Parser t [a]
some p = fmap (:) p <*> many p


optional :: Parser t a -> Parser t (Maybe a)
optional p = fmap Just p <|> pure Nothing


sepBy1 :: Parser t a -> Parser t b -> Parser t ([a], [b])
sepBy1 p s = fmap g p <*> (liftA2 f s (sepBy1 p s) <|> pure ([], []))
  where 
    f a (b, c) = (b, a:c)
    g d (e, f) = (d:e, f)
    

sepBy0 :: Parser t a -> Parser t b -> Parser t ([a], [b])
sepBy0 p s = sepBy1 p s <|> pure ([], [])


end :: Parser t ()
end = switch getOne


not1 :: Parser t b -> Parser t t
not1 p = switch p *> getOne


-- matches if next token is not x, consuming one token
pnot :: Eq t => t -> Parser t t
pnot x = satisfy (/= x)
-- how about:
--  pnot x = not1 (literal x)
--  pnot = not1 . literal


-- matches if next token not in xs, consuming one token
--   not sure if I like this one
pnone :: Eq t => [t] -> Parser t t
pnone xs = satisfy (\x -> not $ elem x xs)
-- how about:
--  pnone xs = not1 (pany $ map literal xs)
--  pnone = not1 . pany . map literal


-- matches all of the tokens in sequence
string :: Eq t => [t] -> Parser t [t]
string = commute . map literal
  
