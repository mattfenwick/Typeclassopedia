{-# LANGUAGE UndecidableInstances #-} -- for P's Semigroup
import Classes

import Prelude hiding (fmap, (>>=), (>>), fail)

-- let q s = mplus (digit s) (lower s)
-- [1..5] >>= \x -> guard (x > 3) >> pure x
-- Just 0.5 >>= \x -> guard (x > 3) >> pure x

f :: (Monad' m, Monoid' (m [Int]), Pointed' m) => m Int -> m [Int]
f m = m >>= \x -> if x < 5 
                  then pure [x - 1 .. x + 1] 
                  else empty


digit :: String -> Maybe Char
digit []      = Nothing
digit (c:_)
  | elem c ['0'..'9'] = Just c
  | otherwise         = Nothing

lower :: String -> Maybe Char
lower [] = Nothing
lower (c:_)
  | elem c ['a'..'z'] = Just c
  | otherwise = Nothing
 


{-
-- | Consume a binary character in the input (i.e. either a 0 or an 1)
binChar :: String -> Maybe Int
binChar s = digit 0 s `mplus` digit 1 s
-}



p1 :: String -> Maybe (Char, String)
p1 ('c':cs) = Just ('c', cs)
p1 _ = Nothing


p2 :: String -> Maybe (Char, String)
p2 ('9':cs) = Just ('9', cs)
p2 _ = Nothing


newtype Parser s a = Parser {p :: [s] -> Maybe ([s], a)}

newtype NParser s a = NParser {getParser :: [s] -> [([s], a)]}


instance Functor' (Parser s) where
  -- left 'fmap' is for Parsers
  -- right 'fmap' 1 is for Maybe
  -- right 'fmap' 2 is for (,)
  fmap f (Parser g) = Parser (fmap (fmap f) . g)

  
instance Pointed' (Parser s) where
  pure x = Parser (\s -> Just (s, x))

  
{- -}
  
instance Applicative' (Parser s) where
-- Parser s (a -> b) ----> :: [s] -> Maybe ([s], a -> b)
-- Parser s a        ----> :: [s] -> Maybe ([s], a)
-- Parser s b        ----> :: [s] -> Maybe ([s], b)
  Parser f <*> Parser g = Parser h
--    where h xs = fmap (\(ys, f') -> fmap (fmap (f' $)) (g ys)) (f xs)
--    () <*> g
    where h xs = case (f xs) of
                      -- fmap 1: for Maybe
                      -- fmap 2: for tuple
                      Just (ys, f') -> fmap (fmap (f' $)) (g ys)
                      Nothing -> Nothing
{-  Parser f <*> Parser g = Parser h
    where
      h xs = 
 -}
 
instance Monad' (Parser s) where
  join (Parser f) = Parser (\xs -> f xs >>= \(o, Parser g) -> g o)
  
  
instance Functor' (NParser s) where
  fmap f (NParser g) = NParser (fmap (fmap f) . g)
  
instance Pointed' (NParser s) where
  pure x = NParser (\s -> [(s, x)])
  
instance Applicative' (NParser s) where
  -- NParser s (a -> b)  ----> :: [s] -> [([s], a -> b)]
  -- NParser s a         ----> :: [s] -> [([s], a)]
  -- NParser s b         ----> :: [s] -> [([s], b)]
--  NParser f <*> NParser g = NParser h
--    where h xs = concatMap (\(ys, f') -> map (fmap (f' $)) (g ys)) (f xs)
--  f <*> v = hmm f v
  NParser f <*> NParser g = NParser h
    where h xs = join (fmap (\(ys, f') -> fmap (fmap (f' $)) (g ys)) (f xs))
  
instance Monad' (NParser s) where
  -- m (m a) -> m a
  -- NParser s (NParser s a) -> NParser s a
  -- NParser s (NParser s a)  ----> :: [s] -> [([s], [s] -> [([s], a)])]
  join (NParser f) = NParser h
    where
      h xs = f xs >>= \(out1, NParser g) -> g out1
    
    
hmm :: (Monad' m) => m (a -> b) -> m a -> m b
hmm mf mv = mf >>= \f -> mv >>= \x -> pure (f x)

 
q = Parser (\ss -> case ss of (x:xs) -> Just (xs, x); _ -> Nothing)
eg1 = ((p q) [1..5], (p q) [])
eg2 = (p (pure (,) <*> q <*> q)) [1..5]
eg3 = (p (pure (,) <*> q <*> q)) [1]

dig :: Int -> Parser Int Int
dig x = Parser (\ss -> case ss of (y:ys) -> if x == y then Just (ys, x) else Nothing; _ -> Nothing)

four = dig 4
five = dig 5

                      
instance Semigroup' (Parser s a) where
  Parser f <|> Parser g = Parser h
    where h xs = case (f xs) of
                      Just (o, ys) -> Just (o, ys);
                      Nothing      -> g xs
                      
instance Monoid' (Parser s a) where
  empty = Parser (const Nothing)




newtype P m s a = P { getP :: [s] -> m ([s], a) }


instance Functor' m => Functor' (P m s) where
  fmap f (P g) = P (fmap (fmap f) . g)

instance Monad' m => Applicative' (P m s) where
  P f <*> P x = P h
    where
      h xs = f xs >>= \(ys, f') -> 
        x ys >>= \(zs, x') ->
        pure (zs, f' x')
        
instance Pointed' m => Pointed' (P m s) where
  pure a = P (\xs -> pure (xs, a))
  
instance Monad' m => Monad' (P m s) where
  join (P f) = P h
    where
      h xs = f xs >>= \(o, P g) -> g o  
  
instance Semigroup' (m ([s], a)) => Semigroup' (P m s a) where
  P f <|> P g = P (\xs -> f xs <|> g xs)
  
instance Monoid' (m ([s], a)) => Monoid' (P m s a) where
  empty = P (const empty)