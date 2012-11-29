{-# LANGUAGE NoMonomorphismRestriction #-}
import Parse.Mt
import Classes
import Prelude     hiding (fmap, (>>=), (>>), fail, foldr, foldl)


s1 x = (tail x, length x + head x)

q y x = (tail x ++ tail x, elem y (tail x))

s2 y = RW (q y)


s3 = 
    RW s1 >>= \x -> 
    s2 x >>= \y ->
    pure (x, y)
    
    
s2' y = RW1 (q y)

s3' =
    RW1 s1 >>= \x ->
    s2' x >>= \y ->
    pure (x, y)