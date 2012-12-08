{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
module Trans.Parse (

    MonadParser(..)
  
  , CntP(..)

) where

import Classes
import Datums
import Instances
import Prelude hiding (foldr, foldl, fmap, (>>=), fail, (>>))
import Trans.MTrans



class (Monad' m) => MonadParser t m | m -> t where
  item :: m t



-- transformer that counts newlines and spaces

newtype CntP m a 
    = CntP {getCntP :: StateT (Int, Int) m a}

instance Functor' m => Functor' (CntP m) where
  fmap f  =  CntP . fmap f . getCntP

instance Pointed' m => Pointed' (CntP m) where
  pure  =  CntP . pure

instance Monad' m => Applicative' (CntP m) where
  CntP f  <*>  CntP x  =  CntP (f <*> x)

instance Monad' m => Monad' (CntP m) where
  join  =  CntP . join . fmap getCntP . getCntP

instance APlus' m => APlus' (CntP m) where
  CntP f  <+>  CntP g  =  CntP (f <+> g)

instance AZero' m => AZero' (CntP m) where
  zero  =  CntP zero
