{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
import Trans.MTrans
import Trans.Instances
import Trans.Combs
import Classes
import Datums
import Prelude hiding ((>>=), (>>), fail, foldr, foldl, fmap)


    

item = 
    get >>= \s -> case s of []     -> zero; 
                            (x:xs) -> put xs >> pure x

                            
k :: StateT [Char] Maybe Char
k = item

e = getStateT k "abcde"


l :: StateT String (StateT Int Maybe) Char
l = item

eg str num = getStateT (getStateT l str) num

eg1 p s n = getStateT (getStateT p s) n