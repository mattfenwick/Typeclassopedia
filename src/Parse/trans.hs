{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe
import Data.Monoid
import Control.Monad (liftM)


type Parser t s a = StateT ([t], s) Maybe a

-- StateT
-- StateT Maybe
-- StateT MaybeT StateT

runParser :: Parser t s a -> ([t],s) -> Maybe (a, ([t],s))
runParser p s = 
    (runStateT p) s

empty :: StateT s Maybe a
empty = StateT (const Nothing)

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

tick :: Parser t Int ()
tick = updateState (+1)


item :: Parser t Int t
item = 
    getTokens >>= \chs -> 
        case chs of
             (c:cs) ->  putTokens cs >> tick >> return c
             []     ->  empty


