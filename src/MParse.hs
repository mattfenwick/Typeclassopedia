module MParse (

    MParser(..)

) where

import Classes
import Instances () -- what does this do?
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)
import IParse


newtype MParser t a = MParser {
    getParser :: [t] -> Maybe ([t], a)
  }


instance Functor' (MParser t) where
  fmap f (MParser g) = MParser (fmap (fmap f) . g)

instance Applicative' (MParser t) where
  f <*> x =
     f   >>= \f' ->
     x   >>= \x' ->
     pure (f' x')

instance Pointed' (MParser t) where
  pure a = MParser (\xs -> pure (xs, a))

instance Monad' (MParser t) where
  join (MParser f) = MParser h
    where
      h xs = 
          f xs >>= \(ys, MParser g) -> 
          g ys  
  
instance Semigroup' (MParser t a) where
  MParser f  <|>  MParser g  =  MParser (\xs -> f xs <|> g xs)
  
instance Monoid' (MParser t a) where
  empty = MParser (const Nothing)
  
instance Switch' (MParser t) where
  switch (MParser f) = MParser h
    where h xs = fmap (const (xs, ())) $ switch (f xs)



instance ParserT MParser where
  
  getOne = MParser h
    where
      h (x:xs)  =  Just (xs, x)
      h []      =  Nothing
  
  check pred p = 
      p >>= \x ->
      guard (pred x) >>
      pure x