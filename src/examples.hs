
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


newtype Parser s a = Parser {p :: ([s] -> Maybe (a, [s]))}


gmap :: (a -> b) -> Maybe a -> Maybe b
gmap = fmap


instance Functor' (Parser s) where
  fmap f (Parser g) = Parser (gmap (\(x, y) -> (f x, y)) . g)
  
  
instance Pointed' (Parser s) where
  pure x = Parser (\s -> Just (x, s))
  
  
instance Applicative' (Parser s) where
-- Parser s (a -> b) ----> :: [s] -> Maybe (a -> b, [s])
-- Parser s a        ----> :: [s] -> Maybe (a, [s])
-- Parser s b        ----> :: [s] -> Maybe (b, [s])
  Parser f <*> Parser g = Parser h
    where h xs = case (f xs) of
                      Just (f', ys) -> case (g ys) of
                                            Just (g', zs) -> Just (f' g', zs);
                                            Nothing -> Nothing;
                      Nothing -> Nothing
                      
                      
instance Semigroup' (Parser s a) where
  Parser f <|> Parser g = Parser h
    where h xs = case (f xs) of
                      Just (o, ys) -> Just (o, ys);
                      Nothing      -> g xs
  