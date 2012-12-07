{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Trans.ParserStack (


) where

import Classes
import Datums
import Instances
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))
import Trans.MTrans
import Trans.Combs
import Trans.Parse
import Trans.Instances


-- type:  [s] -> Either f (Maybe ([s], a))
-- wow, this is weird ,,, somehow it pumps out
-- 'Left ""' on failure ... what's going on?
type P e t a = Parser t (MaybeT (Either e)) a

type PL e t a = Parser t (ListT (Either e)) a


run :: P e t a -> [t] -> Either e (Maybe ([t], a))
run p tokens = getMaybeT (getStateT (getParser p) tokens)


runL :: PL e t a -> [t] -> Either e [([t], a)]
runL p tokens = getListT (getStateT (getParser p) tokens)


run1 :: StateT [t] (MaybeT (Either e)) a -> [t] -> Either e (Maybe ([t], a))
run1 p ts = getMaybeT (getStateT p ts)

run2 :: StateT [t] (ListT (Either e)) a -> [t] -> Either e [([t], a)]
run2 p ts = getListT (getStateT p ts)

{- counter example that shows this is broken:
-- this succeeds
ghci> run2 ((many $ literal 'a') *> literal 'b') "aaabcd"
Right [("cd", 'b')]

-- but this fails -- i.e. adding a 'commit' changed the behavior on success
--   does this indicate the monad stack is in the wrong order ??
--   or that this 'commit' idea is more fundamentally broken?
ghci> run2 ((many $ literal 'a') *> commit (literal 'b')) "aaabcd"
Left "abcd"
-}


-- str :: P String Char String
str = literal '"' *> commit rest
  where
    rest = many (pnot '"') <* literal '"'

commit :: (AOr' m, MonadError e m) => StateT e m a -> StateT e m a
commit p =
    StateT (\ts -> getStateT p ts <||> throwE ts)

com2 p = p <||> lift (throwE)



{- from Trans.Parse
runParser = getStateT . getParser

runCntP :: CntP (Parser t m) a -> (Int, Int) -> [t] -> m ([t], ((Int, Int), a))
runCntP p s ts = getStateT (getParser (getStateT (getCntP p) s)) ts

-- umm ????
parseCntP :: CntP (Parser Char Maybe) Char
parseCntP = item

-- example:
--   ghci> runCntP (many parseCntP) (0, 0) "ab cd \n ef \n   "
--   Just ("",((3,2),"ab cd \n ef \n   "))
-}
