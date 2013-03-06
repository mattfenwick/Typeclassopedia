{-# LANGUAGE Rank2Types #-}
module ClassFree (

) where

import Datums
import Prelude hiding (Functor, fmap)


data Functor f = Functor {_fmap :: forall a b. (a -> b) -> f a -> f b}

fmap :: Functor f -> (a -> b) -> f a -> f b
fmap (Functor g) f xs = g f xs

fList :: Functor []
fList = Functor map

fMaybe :: Functor Maybe
fMaybe = Functor (\f x -> case x of Nothing -> Nothing; Just y -> Just (f y);)

fIO :: Functor IO
fIO = Functor (\f x -> x >>= \y -> return (f y))

fPair :: Functor ((,) c)
fPair = Functor (\f (x, y) -> (x, f y))

fFunc :: Functor ((->) c)
fFunc = Functor (.)

fEither :: Functor (Either c)
fEither = Functor (\f x -> case x of Left e -> Left e; Right y -> Right (f y);)

fState :: Functor (State s)
fState = Functor (\f (State g) -> State (fmap fPair f . g))


data Pointed f = Pointed {_pure :: forall a. a -> f a}

pure :: Pointed f -> a -> f a
pure (Pointed f) x = f x

pList :: Pointed []
pList = Pointed (:[])

pMaybe :: Pointed Maybe
pMaybe = Pointed Just

pIO :: Pointed IO
pIO = Pointed return

-- pPair :: Monoid m -> Pointed ((,) m) ?????

pFunc :: Pointed ((->) c)
pFunc = Pointed const

pEither :: Pointed (Either c)
pEither = Pointed Right

pState :: Pointed (State s)
pState = Pointed (\x -> State (\s -> (s, x)))


data Semigroup a = Semigroup {_plus :: a -> a -> a}

plus :: Semigroup a -> a -> a -> a
plus (Semigroup s) x y = s x y

plusList = Semigroup (++)

plusMaybe = Semigroup (\x y -> case x of (Just a) -> x; _ -> y)