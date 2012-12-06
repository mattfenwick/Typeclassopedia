{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies, 
             FlexibleInstances, UndecidableInstances #-}

import Control.Monad.Trans.State   (StateT(..))
import Control.Monad.State.Class   (MonadState(..))
import Control.Monad.Trans.Maybe   (MaybeT(..))
import Control.Monad.Trans.List    (ListT(..))
import Control.Monad               (MonadPlus(..), guard)

-- stack:
--   StateT
--   MaybeT/ListT
--   Either e


type Parser e t mm a = StateT [t] (mm (Either e)) a


newtype DParser e t a = 
    DParser {getDParser :: Parser e t MaybeT a}

instance Monad (DParser e t) where
  return = DParser . return
  (DParser d) >>= f = DParser (d >>= (getDParser . f))

instance MonadPlus (DParser e t) where
  mzero = DParser (StateT (const (MaybeT (Right Nothing))))
  mplus = undefined   -- will worry about later

instance MonadState [t] (DParser e t) where
  get = DParser get
  put = DParser . put



class (Monad m) => MonadParser t m n | m -> t, m -> n where
  item  :: m t
  parse :: m a -> [t] -> n (a, [t])


class (Monad m, MonadParser t m n) => CommitParser t m n where
  commit :: m a -> m a


instance MonadParser t (DParser e t) (MaybeT (Either e)) where
  item = 
      get >>= \xs -> case xs of
                          (y:ys) -> put ys >> return y;
                          []     -> mzero;
  parse = runStateT . getDParser

instance CommitParser t (DParser [t] t) (MaybeT (Either [t])) where
  commit p =
      DParser (
        StateT (\ts -> MaybeT $ case runMaybeT (parse p ts) of
                            Left e          ->  Left e;
                            Right Nothing   ->  Left ts;
                            Right (Just x)  ->  Right (Just x);))


satisfy f = 
    item >>= \x ->
    guard (f x) >>
    return x


literal x = satisfy (== x)


ab = literal 'a' >> literal 'b'

ab' = literal 'a' >> commit (literal 'b')

myParse :: DParser String Char a -> String -> Either String (Maybe (a, String))
myParse p = runMaybeT . parse p

newtype NParser e t a =
    NParser {getNParser :: Parser e t ListT a}