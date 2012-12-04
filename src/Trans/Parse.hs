{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies, FlexibleInstances #-}
module Trans.Parse (

    MonadParser(..)

  , Parser(..)

  , CntParser(..)
  , parse1
  , runParser

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

instance APlus' m => APlus' (Parser t m) where
  Parser x  <+>  Parser y  =  Parser (x <+> y)

instance AZero' m => AZero' (Parser t m) where
  zero = Parser zero

instance Monad' m => Monad' (Parser t m) where
  -- Parser t m (Parser t m a) -> Parser t m a
  join = Parser . join . fmap getParser . getParser

instance Monad' m => MonadState [t] (Parser t m) where
  get    =  Parser get
  put x  =  Parser (put x)

getOne :: (Monad' m, AZero' m) => Parser t m t
getOne = 
    get >>= \xs -> case xs of 
                   (y:ys)  ->  put ys >> pure y;
                   []      ->  zero; 

instance (AZero' m, Monad' m) => MonadParser t (Parser t m) where
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
-- can I change this to:  :: StateT (Int, Int) m a
--   then for the MonadParser instance, m will be constrained to 
--   be a parser or whatever
newtype CntParser m a 
    = CntParser {getCntParser :: StateT (Int, Int) (Parser Char m) a}
-- here's the stack:
--  StateT (Int, Int)
--  Parser Char
--  m

-- okay, this is cool, but still don't know how to 
-- lift a general parser action into the top level
parserGet :: Monad' m => CntParser m String
parserGet = CntParser (StateT (\ints -> get >>= \cs -> pure (ints, cs)))

parserPut :: Monad' m => String -> CntParser m ()
parserPut cs = CntParser (StateT (\ints -> put cs >> pure (ints, ())))


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

instance APlus' m => APlus' (CntParser m) where
  CntParser f  <+>  CntParser g  =  CntParser (f <+> g)

instance AZero' m => AZero' (CntParser m) where
  zero  =  CntParser zero

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


instance (AZero' m, Monad' m) => MonadParser Char (CntParser m) where
  item =
      parserGet >>= \xs -> case xs of
                           (' ':ys)  -> get             >>= \(s, n) -> 
                                        put (s + 1, n)  >> 
                                        parserPut ys    >> 
                                        pure ' ';
                           ('\n':ys) -> get             >>= \(s, n) -> 
                                        put (0, n + 1)  >> 
                                        parserPut ys    >> 
                                        pure '\n';
                           (y:ys)    -> parserPut ys    >>
                                        pure y;
                           []        -> zero;


runParser :: CntParser m a -> (Int, Int) -> String -> m (String, ((Int, Int), a))
runParser p s ts = getStateT (getParser (getStateT (getCntParser p) s)) ts


parse1 :: CntParser Maybe Char
parse1 = item
