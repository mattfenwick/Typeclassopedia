module Parse.SParse (

    SParser(..)

) where

import Classes
import Instances () -- what does this do?
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)
import Parse.IParse


newtype SParser s t a = SParser {
    getParser :: ([t], s) -> Maybe (([t], s), a)
  }


instance Functor' (SParser s t) where
  fmap f (SParser g) = SParser (fmap (fmap f) . g)

instance Applicative' (SParser s t) where
  f <*> x =
     f   >>= \f' ->
     x   >>= \x' ->
     pure (f' x')

instance Pointed' (SParser s t) where
  pure a = SParser (\q -> pure (q, a))

instance Monad' (SParser s t) where
  join (SParser f) = SParser h
    where
      h q = 
          f q >>= \(q', SParser g) -> 
          g q'
  
instance Semigroup' (SParser s t a) where
  SParser f  <|>  SParser g  =  SParser (\q -> f q <|> g q)
  
instance Monoid' (SParser s t a) where
  empty = SParser (const Nothing)
  
instance Switch' (SParser s t) where
  switch (SParser f) = SParser h
    where h q = fmap (const (q, ())) $ switch (f q)


get :: SParser s t s
get = SParser h
  where
    h (xs, s) = Just ((xs, s), s)


put :: s -> SParser s t ()
put s' = SParser h
  where
    h (xs, s) = Just ((xs, s'), ())


update :: (s -> s) -> SParser s t ()
update f =
    get        >>=
    (put . f)



instance ParserT (SParser s) where
  
  getOne = SParser h
    where
      h ((x:xs), s)  =  Just ((xs, s), x)
      h ( [], _)     =  Nothing
  
  check pred p = 
      p >>= \x ->
      guard (pred x) >>
      pure x