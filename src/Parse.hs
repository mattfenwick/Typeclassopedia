{-# LANGUAGE UndecidableInstances #-} -- for P's Semigroup
{-# LANGUAGE NoMonomorphismRestriction #-} -- for 'four' and 'five'

module Parse (

) where

import Classes
import Prelude hiding (fmap, (>>=), (>>), fail)




newtype Parser m s a = Parser { getParser :: [s] -> m ([s], a) }


instance Functor' m => Functor' (Parser m s) where
  fmap f (Parser g) = Parser (fmap (fmap f) . g)

instance Monad' m => Applicative' (Parser m s) where
  Parser f <*> Parser x = Parser h
    where
      h xs = f xs >>= \(ys, f') -> 
        x ys >>= \(zs, x') ->
        pure (zs, f' x')
        
instance Pointed' m => Pointed' (Parser m s) where
  pure a = Parser (\xs -> pure (xs, a))
  
instance Monad' m => Monad' (Parser m s) where
  join (Parser f) = Parser h
    where
      h xs = f xs >>= \(o, Parser g) -> g o  
  
instance Semigroup' (m ([s], a)) => Semigroup' (Parser m s a) where
  Parser f <|> Parser g = Parser (\xs -> f xs <|> g xs)
  
instance Monoid' (m ([s], a)) => Monoid' (Parser m s a) where
  empty = Parser (const empty)
  
instance (Functor' m, Switch' m) => Switch' (Parser m s) where
  switch (Parser f) = Parser h
    where h xs = fmap (const (xs, ())) $ switch (f xs) 



-- --------------------------------------------

-- always succeeds, consuming no input
succeed :: Pointed' m => a -> Parser m s a
succeed = pure


-- always fails
pfail :: Monoid' a => a
pfail = empty
  
  
-- succeeds, consuming one 'token', as
--   long as input is not empty
getOne :: (Pointed' m, Monoid' (m ([s], s))) => Parser m s s
getOne = Parser (\xs -> case xs of 
                             (y:ys) -> pure (ys, y);
                             _      -> empty;)
  
  
check :: (Monad' m, Monoid' (m ())) => (a -> Bool) -> m a -> m a
check f p = p >>= \x -> 
  guard (f x) >> 
  pure x


satisfy :: (Monad' m, Monoid' (m ([a], a)), Monoid' (m ([a], ()))) => (a -> Bool) -> Parser m a a
satisfy p = check p getOne
  
  
literal :: (Eq a, Monad' m, Monoid' (m ([a], a)), Monoid' (m ([a], ()))) => a -> Parser m a a
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
many :: (Pointed' f, Semigroup' (f [a]), Applicative' f) => f a -> f [a]
many p = some p <|> pure []

        
-- match parser 1 or more times
some :: (Pointed' f, Semigroup' (f [a]), Applicative' f) => f a -> f [a]
some p = fmap (:) p <*> many p


pany = mconcat


pall :: (Pointed' f, Applicative' f) => [f a] -> f [a]
pall [] = pure []
pall (x:xs) = fmap (:) x <*> pall xs


optional :: (Functor' f, Pointed' f, Semigroup' (f (Maybe a))) => f a -> f (Maybe a)
optional p = (fmap Just p) <|> pure Nothing


sepBy1 :: (Applicative' f, Pointed' f, Semigroup' (f ([a1], [a]))) => f a1 -> f a -> f ([a1], [a])
sepBy1 p s = fmap g p <*> (liftA2 f s (sepBy1 p s) <|> pure ([], []))
  where 
    f a (b, c) = (b, a:c)
    g d (e, f) = (d:e, f)
    

sepBy0 :: (Applicative' f, Pointed' f, Semigroup' (f ([a1], [a]))) => f a1 -> f a -> f ([a1], [a])
sepBy0 p s = sepBy1 p s <|> pure ([], [])


end :: (Pointed' m, Monoid' (m ([a], a)), Functor' m, Switch' m) => Parser m a ()
end = switch getOne


not0 :: (Switch' f) => f a -> f ()
not0 = switch


not1 :: (Monad' m, Switch' m, Monoid' (m ([b], b))) => Parser m b a -> Parser m b b
not1 p = not0 p *> getOne


-- matches if next token is not x, consuming one token
pnot :: (Eq a, Monad' m, Monoid' (m ([a], a)), Monoid' (m ([a], ()))) => a -> Parser m a a
pnot x = satisfy (/= x)
-- how about:
--  pnot x = not1 (literal x)
--  pnot = not1 . literal


-- matches if next token not in xs, consuming one token
--   not sure if I like this one
pnone :: (Eq a, Monad' m, Monoid' (m ([a], a)), Monoid' (m ([a], ()))) => [a] -> Parser m a a
pnone xs = satisfy (\x -> not $ elem x xs)
-- how about:
--  pnone xs = not1 (pany $ map literal xs)
--  pnone = not1 . pany . map literal


-- matches all of the tokens in sequence
string :: (Monad' m, Eq a, Monoid' (m ([a], a)), Monoid' (m ([a], ()))) => [a] -> Parser m a [a]
string = pall . map literal

  
{- 
-- changes error message if parser fails
message :: String -> Parser a b -> Parser a b
message m p inp = tryIt (p inp)
  where tryIt (Left (_, x)) = pfail [m] x
        tryIt y = y


addAlt :: String -> Parser a b -> Parser a b
addAlt a p inp = hmm (p inp)
  where hmm (Left (as, x)) = Left ((a:as), x)
        hmm x = x
        
        
addAlts :: [String] -> Parser a b -> Parser a b
addAlts as p inp = hmm (p inp)
  where hmm (Left (oass, x)) = Left (oass ++ as, x)
        hmm x = x
  
  
-- allow a function applied to the result
--   cause the parser to fail
usingFail :: (b -> Either (Failure a) c) -> Parser a b -> Parser a c
usingFail f p inp = p inp >>= 
  \(rest, r) -> f r >>=
  \c -> return (rest, c)
  
-- ---------------------------------------------------
-- experiments


-- succeed but consume no input if 'p' succeeds
--   this is a very weird combinator and
--   I don't feel comfortable with it
lookahead :: Parser a b -> Parser a ()
lookahead p inp = tryIt $ p inp
  where tryIt (Right _) = succeed () inp
        tryIt (Left _) = pfail ["'lookahead' predicate"] inp
        
        
lookahead :: Parser m s a -> Parser m s ()
lookahead = Parser h
  where
    h xs = (

-- ---------------------------------------------------
-- parsing fun

digits :: [Parser Char Char]
digits = map literal ['0'..'9']

digit :: Parser Char Char
digit = pany digits

integer :: Parser Char Integer
integer = using read $ some digit

alpha :: Parser Char Char
alpha = pany $ map literal (['a' .. 'z'] ++ ['A' .. 'Z'])

wschar :: Parser Char Char
wschar = pany $ map literal " \t\n\r\f"

-}


-- --------------------------------------------


q = Parser (\ss -> case ss of (x:xs) -> Just (xs, x); _ -> Nothing)
eg1 = ((getParser q) [1..5], (getParser q) [])
eg2 = (getParser (pure (,) <*> q <*> q)) [1..5]
eg3 = (getParser (pure (,) <*> q <*> q)) [1]

-- oops ... screwed this up
dig :: (Pointed' m, Monoid' (m ([Int], Int))) => Int -> Parser m Int Int
dig x = Parser (\ss -> case ss of 
                            (y:ys) -> if x == y 
                                      then pure (ys, x) 
                                      else empty;
                            _ -> empty)

four = dig 4
five = dig 5