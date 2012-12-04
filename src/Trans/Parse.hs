{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
module Trans.Parse (

    MonadParser(..)

  , Parser(..)
  , runParser
  
  , CntP(..)
  , runCntP
  , parseCntP

) where

import Classes
import Datums
import Instances
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))
import Trans.MTrans



class (Monad' m) => MonadParser t m | m -> t where
  item :: m t


-- base parser

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

instance (AZero' m, Monad' m) => MonadParser t (Parser t m) where
  -- StateT [t] m t
  item = 
    get >>= \xs -> case xs of 
                   (y:ys)  ->  put ys >> pure y;
                   []      ->  zero; 

instance MonadTrans' (Parser t) where
  -- m a -> Parser t m a
  -- m a -> StateT [t] m a
  -- m a -> [t] -> m ([t], a)
  lift m = Parser h
    where
      h = StateT (\ts -> m >>= \x -> pure (ts, x))

runParser = getStateT . getParser


-- ----------------------------------------------------------------------
-- transformer that counts newlines and spaces

newtype CntP m a 
    = CntP {getCntP :: StateT (Int, Int) m a}

instance Functor' m => Functor' (CntP m) where
  fmap f  =  CntP . fmap f . getCntP

instance Pointed' m => Pointed' (CntP m) where
  pure  =  CntP . pure

instance Monad' m => Applicative' (CntP m) where
  CntP f  <*>  CntP x  =  CntP (f <*> x)

instance Monad' m => Monad' (CntP m) where
  join  =  CntP . join . fmap getCntP . getCntP

instance Monad' m => MonadState (Int, Int) (CntP m) where
  get  =  CntP  get
  put  =  CntP  .  put

instance APlus' m => APlus' (CntP m) where
  CntP f  <+>  CntP g  =  CntP (f <+> g)

instance AZero' m => AZero' (CntP m) where
  zero  =  CntP zero

instance MonadTrans' CntP where
  -- m a -> CntP m a
  -- m a -> StateT (Int, Int) m a
  -- m a -> (Int, Int) -> m ((Int, Int), a)
  lift m = CntP h
    where
      h = StateT (\ints -> m >>= \x -> pure (ints, x))

-- the state is (space indentation, line number)
-- passing a '\n' resets the space to zero
-- this ignores tabs -- just lazy
instance (MonadParser Char m) => MonadParser Char (CntP m) where
  item =
      get        >>= \(ss, ns) ->
      lift item  >>= \x -> case x of
                                ' '  -> put (ss + 1, ns) >> pure x;
                                '\n' -> put (0, ns + 1)  >> pure x;
                                y    -> pure y;



runCntP :: CntP (Parser t m) a -> (Int, Int) -> [t] -> m ([t], ((Int, Int), a))
runCntP p s ts = getStateT (getParser (getStateT (getCntP p) s)) ts

-- umm ????
parseCntP :: CntP (Parser Char Maybe) Char
parseCntP = item

-- example:
--   ghci> runCntP (many parseCntP) (0, 0) "ab cd \n ef \n   "
--   Just ("",((3,2),"ab cd \n ef \n   "))
