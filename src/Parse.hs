{-# LANGUAGE UndecidableInstances #-} -- for P's Semigroup
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