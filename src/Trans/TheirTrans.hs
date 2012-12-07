{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies, 
             FlexibleInstances, UndecidableInstances #-}

import Control.Monad.Trans.State   (StateT(..))
import Control.Monad.State.Class   (MonadState(..))
import Control.Monad.Identity      (Identity)
import Control.Monad.Trans.Error   (ErrorT(..))
import Control.Monad.Error.Class   (MonadError(..))
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


class MonadPlus m => MonadOr m where
  morelse :: m a -> m a -> m a

instance MonadOr [] where
  morelse  []  r  =  r
  morelse  l   _  =  l

instance MonadOr Maybe where
  morelse  Nothing   r  =  r
  morelse  (Just x)  _  =  Just x

instance MonadOr m => MonadOr (StateT s m) where
  -- MonadOr m => 
  -- StateT s m a -> StateT s m a -> StateT s m a
  -- (s -> m (a, s)) -> (s -> m (a, s)) -> s -> m (a, s)
  morelse l r = StateT (\s -> morelse (runStateT l s) (runStateT r s))

instance Monad m => MonadOr (MaybeT m) where
  -- m (Maybe a) -> m (Maybe a) -> m (Maybe a)
  morelse (MaybeT l) (MaybeT r) =
      MaybeT (l >>= \x ->
                case x of Nothing  ->  r
                          Just y   ->  return x)

instance MonadOr (DParser e t) where
  -- DParser e t a -> DParser e t a -> DParser e t a
  morelse (DParser l) (DParser r) = DParser (morelse l r)

instance Monad m => MonadOr (ListT m) where
  -- m [a] -> m [a] -> m [a]
  morelse (ListT l) (ListT r) = 
      ListT (l >>= \x ->
               case x of []      ->  r
                         (y:ys)  ->  return x)

com p =
  morelse p (throwError "oops!")

com2 p = 
  DParser (StateT (\s -> morelse (runStateT (getDParser p) s) 
                                 (throwError s)))

com3 p = 
  morelse p (DParser (StateT throwError))


instance MonadParser t (DParser e t) (MaybeT (Either e)) where
  item = 
      get >>= \xs -> case xs of
                          (y:ys) -> put ys >> return y;
                          []     -> mzero;
  parse = runStateT . getDParser

{- -- just curious 
instance MonadError e (StateT s m) where
  throwError = undefined
  catchError = undefined -}

instance MonadError e (DParser e t) where
  -- e -> m a
  -- e -> DParser e t a
  -- e -> StateT [t] (MaybeT (Either e)) a
  -- e -> [t] -> Either e (Maybe (a, [t]))
  -- TODO is there no better way to do this?
  throwError e = DParser (StateT (\s -> MaybeT (Left e)))
  catchError = error "worry about this later"

satisfy f = 
    item >>= \x ->
    guard (f x) >>
    return x


literal x = satisfy (== x)

commit p = 
  DParser (StateT (\xs -> mplus (runStateT (getDParser p) xs)
                                (throwError xs)))


ab = literal 'a' >> literal 'b'

ab' = literal 'a' >> commit (literal 'b')

myParse :: DParser String Char a -> String -> Either String (Maybe (a, String))
myParse p = runMaybeT . parse p



newtype NParser e t a =
    NParser {getNParser :: Parser e t ListT a}

instance Monad (NParser e t) where
  return = NParser . return
  (NParser d) >>= f = NParser (d >>= (getNParser . f))

instance MonadPlus (NParser e t) where
  mzero = NParser (StateT (const (ListT (Right []))))
  mplus = undefined   -- will worry about later

instance MonadState [t] (NParser e t) where
  get = NParser get
  put = NParser . put


instance MonadParser t (NParser e t) (ListT (Either e)) where
  item = 
      get >>= \xs -> case xs of
                          (y:ys) -> put ys >> return y;
                          []     -> mzero;
  parse = runStateT . getNParser

cd' = literal 'c' >> commit' (literal 'd')

commit' p =
  NParser (StateT (\xs -> morelse (runStateT (getNParser p) xs)
                                  (throwError xs)))

myParse' :: NParser String Char a -> String -> Either String [(a, String)]
myParse' p = runListT . parse p
