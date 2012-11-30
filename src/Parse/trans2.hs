{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad.Trans.State (StateT(..), runStateT, get, put)
import Control.Monad (liftM)
import Control.Applicative (Alternative(..))


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


item :: (Monad m, Alternative m) => Parser t Int m t
item = 
    getTokens >>= \toks -> 
        case toks of
             (c:cs) ->  putTokens cs >> tick >> return c
             []     ->  aempty


class MonadParser t where
  getOne :: Parser t s m a



newtype ParseM t m a
    = ParseM {
        getParseM :: StateT [t] m a
    -- getParseM :: [t] -> m (a, [t]) -- is that the same thing?
    }

instance ParserT ParseM
