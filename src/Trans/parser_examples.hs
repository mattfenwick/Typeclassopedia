{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

import Classes
import Datums
import Instances
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))
import Trans.MTrans
import Trans.Combs
import Trans.Instances


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

com3 p = get >>= \ts -> (getStateT p ts >>= \(us, v) -> put us >> pure v)
                        <||> throwE ts

com4 p = p <||> StateT throwE
