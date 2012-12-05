{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
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


run :: P e t a -> [t] -> Either e (Maybe ([t], a))
run p tokens = getMaybeT (getStateT (getParser p) tokens)


str :: P String Char String
str = literal '"' *> commit rest
  where
    rest = many (pnot '"') <* literal '"'



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
