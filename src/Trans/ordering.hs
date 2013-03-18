{-# LANGUAGE NoMonomorphismRestriction #-}

import Classes
import Datums
import Instances

import Trans.MTrans
import Trans.Instances

import Prelude hiding ((>>), (>>=), fail, foldr, fmap, foldl)


-- s -> Maybe (s, (w, a))
-- Int -> Maybe (Int, (String, Float))
-- backtracking affects both the writer and the state
u :: WriterT String (StateT Int Maybe) Float
u = pure 3


f (WriterT (StateT w)) s = w s

a1 = f u 13
a2 = f ( write "poopy pants" >> u) 13   --  I want my log !!!

{-
StateT
Maybe  -->  s -> m (s, a)

MaybeT
State  -->  s -> (s, Maybe a)
-}
-- s -> (w, Maybe (s, a))
-- Int -> (String, Maybe (Int, Float))
-- backtracking does not affect the writer, but does affect the state
no :: StateT Int (MaybeT (WriterT String Id)) Float
no = pure 14.3

getId (Id x) = x
-- s -> 
g :: StateT s (MaybeT (WriterT w Id)) a -> s -> (w, Maybe (s, a))
g (StateT f) = getId . getWriterT . getMaybeT . f

-- par :: 
par = 
    get >>= \xs -> case xs of 
                       (t:ts) -> put ts >> pure t; 
                       []     -> zero;

q1 = (par >> zero >> par) <+> par

-- state IS backtracked
q2 :: StateT [Char] Maybe Char
q2 = q1
rQ2 :: [Char] -> Maybe ([Char], Char)
rQ2 = getStateT q2

-- state IS NOT backtracked
q3 :: MaybeT (State [Char]) Char
q3 = q1
rQ3 :: [Char] -> ([Char], Maybe Char)
rQ3 = getState (getMaybeT q3)


-- problem:  parse any number of a's, no more than 6 b's, and end at the 2nd c
x' <* y' = pure const <*> x' <*> y'
until' p f = 
    p         >>= \x ->
    lift get  >>= \s ->
      if (f s) 
      then ((until' p f >>= \xs -> pure (x:xs)) <+> pure [x])
      else pure [x]

cPar c = par >>= \x -> guardA (x == c) >> pure x
a = cPar 'a'
b = cPar 'b' <* (lift get >>= \(b, c) -> if b < 6 then (lift $ put (b + 1, c)) else throwE "too many b's")
c = cPar 'c' <* (lift get >>= \(b, c) -> lift $ put (b, c + 1))

type Parser a = StateT [Char] 
                  (StateT (Int, Int) 
                          (MaybeT (Either String))) 
                  a

chs2 :: Parser String
chs2 = until' (a <+> b <+> c) (\(_, c) -> c < 2)

run :: Parser a -> [Char] -> (Int, Int) -> Either String (Maybe ((Int, Int), ([Char], a)))
run p xs st = getMaybeT (getStateT (getStateT p xs) st)
