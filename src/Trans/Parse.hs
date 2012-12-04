{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies, UndecidableInstances, FlexibleInstances #-}
module Trans.Parse (

) where

import Classes
import Datums
import Instances
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))
import Trans.MTrans



class (Monad' m) => MonadParser t m | m -> t where
  item :: m t



newtype Parser t m a
    = Parser {getParser :: StateT [t] m a}

instance Functor' m => Functor' (Parser t m) where
  -- (a -> b) -> Parser t m a -> Parser t m b
  fmap f = Parser . fmap f . getParser

instance Pointed' m => Pointed' (Parser t m) where
  pure = Parser . pure

instance Monad' m => Applicative' (Parser t m) where
  Parser f <*> Parser x = Parser (f <*> x)

instance Semigroup' (m ([t], a)) => Semigroup' (Parser t m a) where
  Parser f <|> Parser g = Parser (f <|> g)

instance Monoid' (m ([t], a)) => Monoid' (Parser t m a) where
  empty = Parser empty

instance Monad' m => Monad' (Parser t m) where
  -- Parser t m (Parser t m a) -> Parser t m a
  join = Parser . join . fmap getParser . getParser

instance Monad' m => MonadState [t] (Parser t m) where
  get    =  Parser get
  put x  =  Parser (put x)

getOne :: (Monad' m, Monoid' (m ([t], t))) => Parser t m t
getOne = 
    get >>= \xs -> case xs of 
                   (y:ys)  ->  put ys >> pure y;
                   []      ->  Parser empty; 

instance (Monoid' (m ([t], t)), Monad' m) => MonadParser t (Parser t m) where
  -- StateT [t] m t
  item = getOne

instance MonadTrans' (Parser t) where
  -- m a -> Parser t m a
  -- m a -> StateT [t] m a
  -- m a -> [t] -> m ([t], a)
  lift m = Parser h
    where
      h = StateT (\ts -> m >>= \x -> pure (ts, x))



-- a 'count' parser
-- <=== counts newlines and spaces ... maybe just
--   has to wrap/unwrap the StateT instances for Functor, Pointed, etc.?
newtype CntParser m a 
    = CntParser {getCntParser :: StateT (Int, Int) (Parser Char m) a}


instance Functor' m => Functor' (CntParser m) where
  fmap f = CntParser . fmap f . getCntParser

instance Pointed' m => Pointed' (CntParser m) where
  pure = CntParser . pure

instance Monad' m => Applicative' (CntParser m) where
  CntParser f <*> CntParser x = CntParser (f <*> x)

instance Monad' m => Monad' (CntParser m) where
  join = CntParser . join . fmap getCntParser . getCntParser

instance Monad' m => MonadState (Int, Int) (CntParser m) where
  get  =  CntParser  get
  put  =  CntParser  .  put

instance MonadTrans' CntParser where
  -- m a -> CntParser m a
  -- m a -> StateT (Int, Int) (Parser Char m) a
  -- m a -> (Int, Int) -> (Parser Char m) ((Int, Int), a)
  -- m a -> (Int, Int) -> String -> m (String, ((Int, Int), a))
  -- so just need to grab the two states and tuple 'em up or
  -- whatever with the value from the monad ... why is this
  -- so f***ing hard to do?
  lift m = CntParser h
    where
      h = StateT (\ints -> Parser (StateT (\s -> m >>= \x -> pure (s, (ints, x)))))

liftMe :: Parser Char m a -> CntParser m a
-- Parser Char m a -> StateT (Int, Int) (Parser Char m) a
-- ... -> (Int, Int) -> Parser Char m ((Int, Int), a)
-- (String -> m (String, a)) -> (Int, Int) -> String -> m (String, ((Int, Int), a))
liftMe = error "undefined !!!"

{-
instance (Monoid' (m (String, ((Int, Int), Char))), Monad' m) => MonadParser Char (CntParser m) where
  -- CntParser t m t
  item =
      lift get >>= \xs -> case xs of
                          (' ':ys)  -> get             >>= \(s, n) -> 
                                       put (s + 1, n)  >> 
                                       lift (put ys)   >> 
                                       pure ' ';
                          ('\n':ys) -> get             >>= \(s, n) -> 
                                       put (0, n + 1)  >> 
                                       lift (put ys)   >> 
                                       pure '\n';
                          []        -> CntParser empty;

runParser :: CntParser m a -> (Int, Int) -> String -> m (String, ((Int, Int), a))
runParser p s ts = getStateT (getParser (getStateT (getCntParser p) s)) ts

parse1 :: CntParser (Parser Char Maybe) Char
parse1 = item
-}
