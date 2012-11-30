{-# LANGUAGE UndecidableInstances, FlexibleInstances, NoMonomorphismRestriction, MultiParamTypeClasses, FunctionalDependencies #-}
import Control.Monad.Trans.State (StateT(..), runStateT, get, put)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad (liftM, guard, MonadPlus, mzero, mplus)
import Control.Applicative (Alternative(..))
import Control.Monad.Trans.Class (MonadTrans(..))



type Parser t s m a = StateT ([t], s) m a

-- StateT Maybe

runParser :: Parser t s m a -> ([t],s) -> m (a, ([t],s))
runParser p s = 
    (runStateT p) s

aempty :: Alternative m => StateT s m a
aempty = StateT (const empty)


getTokens = liftM fst get

putTokens ts =
    get >>= \(_,s) ->
    put (ts, s)

updateTokens f =
    getTokens >>= \ts ->
    putTokens (f ts)

getState = liftM snd get

putState s' = 
    get >>= \(ts,_) ->
    put (ts, s')

updateState f =
    getState >>= \x ->
    putState (f x)


tick :: Monad m => Parser t Int m ()
tick = updateState (+1)


item' :: (Monad m, Alternative m) => Parser t Int m t
item' = 
    getTokens >>= \toks -> 
        case toks of
             (c:cs) ->  putTokens cs >> tick >> return c
             []     ->  aempty


newtype ParserT t m a
    = ParserT {
          getParserT :: StateT [t] m a
    -- :: [t] -> m (a, [t]) -- is that the same thing?
    }

instance Monad m => Monad (ParserT t m) where
  return x = ParserT (StateT (\xs -> return (x, xs)))
  a >>= b = undefined

instance MonadPlus m => MonadPlus (ParserT t m) where
  mzero = ParserT (StateT (const mzero))
  mplus a b = undefined


getOneT :: MonadPlus m => [t] -> m (t, [t])
getOneT (x:xs) = return (x, xs)
getOneT []     = mzero

{- satisfyT :: MonadPlus m => (a -> Bool) -> ([t] -> m (a, [t])) -> [t] -> m (a, [t])
satisfyT f p xs =
    p xs >>= \(x, xs') ->
    guard (f x) >>
    return (x, xs') -}


class MonadParser t m | m -> t where
  getOne :: [t] -> m (t, [t])
--  satisfy :: (a -> Bool) -> [t] -> m (a, [t])

instance MonadPlus m => MonadParser t (ParserT t m) where
  getOne = getOneT

instance MonadTrans (ParserT t) where
  lift m = ParserT (StateT h)
    where
      h xs = 
          m >>= \a ->
          return (a, xs)

-- why do I need a Monad constraint on m here?
-- and what is this code doing?
instance (MonadParser t m, Monad m) => MonadParser t (StateT s m) where
  getOne = lift . getOne
  