module TParse (

    TParser(..)

  , getOne
  , check
  
  , commit

  , (<?>)

) where

import Classes
import Instances () -- what does this do?
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)
import Thing
import IParse


-- ideas for dealing with errors:
--   1. in a 'commit', record where the commit started
--      and how far the parsing got before it failed
--   2. maybe forget the idea of having all 'stack trace'
--      be automatically managed, but instead, whenever
--      the user adds a context message to a parser, push
--      that message on to a stack if the parser fails
--   3. or just return all possible failure points (even
--      ones that were successfully backtracked from ??)
--      and leave it up to the programmer to use 'commit'
--      to pare down the tree of possibilities
--   4. but remember that if I want to be able to read the
--      position information, and there's more than 1
--      parsing stage, I'll have to preserve that information
--      to be able to deliver a decent error message
--   5. ways to do that:  a) augment all tokens with column,
--      line info -- but how does that interact with multi-
--      stage parsers?? -- b) keep track of the columns, 
--      lines inside the parser -- same ? as for a ...

mapFail :: (b -> d) -> Thing a b c -> Thing a d c
mapFail f (Fail x)    =  Fail (f x)
mapFail _ (Error a)   =  Error a
mapFail _ (Ok b)      =  Ok b


mapError :: (a -> e) -> Thing a b c -> Thing e b c
mapError f (Error a)   =  Error (f a)
mapError _ (Fail b)    =  Fail b
mapError _ (Ok c)      =  Ok c


mapFE :: (a -> c) -> Thing a a b -> Thing c c b
mapFE f = mapFail f . mapError f


(<?>) :: String -> TParser t a -> TParser t a
name <?> p = TParser h
  where
    h xs = mapFE (\(ns,ts,us) -> (name:ns,ts,us)) (getParser p xs)



data TParser t a = TParser {
      getParser ::  [t] -> 
          Thing ([String], [t], [t]) 
                ([String], [t], [t]) 
                ([t], a) 
  }




instance Functor' (TParser s) where
  fmap f (TParser g) = TParser (fmap (fmap f) . g)

instance Applicative' (TParser s) where
  f <*> x =
     f   >>= \f' ->
     x   >>= \x' ->
     pure (f' x')
        
instance Pointed' (TParser s) where
  pure a = TParser (\xs -> pure (xs, a))

instance Monad' (TParser s) where
  join (TParser f) = TParser h
    where
      h xs = 
          f xs >>= \(o, TParser g) -> 
          g o  
  
instance Semigroup' (TParser s a) where
  TParser f  <|>  TParser g  =  TParser (\xs -> f xs <|> g xs)
  
instance Monoid' (TParser s a) where
  empty = TParser (Fail . (,,) [] [])
  
instance Switch' (TParser s) where
  switch (TParser f) = TParser (\xs -> h xs (f xs))
    where h xs (Ok _)     =  Fail ([], xs, xs)
          h xs (Fail _)   =  Ok (xs, ())
          h _  (Error z)  =  Error z


instance ParserT TParser where

  getOne = TParser (\xs -> case xs of 
                          (y:ys) -> pure (ys, y);
                          _      -> Fail ([], xs, xs))

-- have to preserve the original position
-- in token stream for error reporting, 
-- otherwise this would be a lot simpler  
  check pred p = TParser h
    where 
      h xs = mapFE (f xs) (getParser parser xs)
      f ys (ns, _, _) = (ns, ys, ys)
      parser =
          p >>= \x -> 
          guard (pred x) >> 
          pure x


-- ------------------------

-- should I add something in here
-- to make it keep track of where
-- it was in the token stream when
-- it started the parser?  (in case
-- it fails, for reporting)
commit :: TParser t a -> TParser t a
commit p = TParser (\xs -> h xs $ getParser p xs)
  where
    h xs (Fail (ts,_,ys))   =  Error (ts,xs,ys)
    h _  x                  =  x
