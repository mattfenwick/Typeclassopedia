{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
module Trans.Parse (

    MonadParser(..)
  , CommitParser(..)

  , Parser(..)
  
  , CntP(..)

) where

import Classes
import Datums
import Instances
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))
import Trans.MTrans



class (Monad' m) => MonadParser t m | m -> t where
  item :: m t


class MonadParser t m => CommitParser t m | m -> t where
  commit :: m a -> m a



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

instance APlus' m => APlus' (CntP m) where
  CntP f  <+>  CntP g  =  CntP (f <+> g)

instance AZero' m => AZero' (CntP m) where
  zero  =  CntP zero
