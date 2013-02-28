{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
import Trans.MTrans
import Trans.Instances
import Trans.Combs
import Classes
import Datums
import Prelude hiding ((>>=), (>>), fail, foldr, foldl, fmap)


    
                            
k :: StateT [Char] Maybe Char
k = item

e = getStateT k "abcde"


l :: StateT String (StateT Int Maybe) Char
l = item

eg str num = getStateT (getStateT l str) num

eg1 p s n = getStateT (getStateT p s) n

-- example:  getStateT (getStateT (state2 (\x -> x ++ x) >> state2 tail) 14) "abcdefg"
state2 f =
    get               >>= \r ->
    lift get          >>= \s ->
    put (r + 1)       >>
    lift (put $ f s)  >>
    pure (r, s)
    