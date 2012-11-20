module Bananas (

) where

import Control.Arrow ((***))
import Prelude hiding ((^), (||))


-- type variable conventions:
--  a:  element of list
--  b:  base element

-- name conventions:
--  build: in: list element, base element; out: new base element
--  base: base case, default value
--  x, xs:  first, rest of list
--  pred: a function that returns a boolean
--  next: a function doing something .... ????


cat :: (a -> b -> b) -> b -> [a] -> b
cat build base = h
  where
    h []       =  base
    h (x:xs)   =  build x (h xs)


ana :: (b -> Maybe (a, b)) -> b -> [a]
ana next base2 = h (next base2)
  where
    h Nothing              =   []
    h (Just (x, base2'))   =   x : h (next base2')


hylo :: (a -> b -> b) -> b -> (b' -> Maybe (a, b')) -> b' -> b
hylo build base next base2 = h (next base2)
  where
    h Nothing              =   base
    h (Just (x, base2'))   =   build x (h (next base2'))


para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para f base = h
  where
    h []       =   base
    h (x:xs)   =   f x xs (h xs)



ana' :: (b -> Bool) -> (b -> (a, b)) -> b -> [a]
ana' pred next = h
  where
    h base
      | pred base   =  []
      | otherwise   =  let 
                         (x, base') = next base
                       in (x : h base')


hylo' :: (a -> b -> b) -> b -> (b' -> Bool) -> (b' -> (a, b')) -> b' -> b
hylo' build base pred next = h
  where
    h base'
      | pred base'   =  base
      | otherwise    =  let
                          (x, base'') = next base'
                        in build x (h base'')


para' :: b -> (a -> ([a], b) -> b) -> [a] -> b
para' base f []       =   base
para' base f (x:xs)   =   f x (xs, para' base f xs)



-- ----------------
-- ADT section

(||) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(||) = (***)

dup :: a -> (a, a)
dup x = (x, x)

piLeft :: (a, b) -> a
piLeft = fst

piRight :: (a, b) -> b
piRight = snd

(^) :: (a -> b) -> (a -> d) -> a -> (b, d)
f ^ g = (f *** g) . dup

(?|?) :: (a -> b) -> (c -> d) -> Either a c -> Either b d
f ?|? g = \x -> case x of
                (Left x'') -> Left (f x'');
                (Right x') -> Right (g x');

leftI :: a -> Either a b
leftI = Left

rightI :: b -> Either a b
rightI = Right

v :: (a -> d) -> (c -> d) -> Either a c -> d
v f g (Left x)   =  f x
v f g (Right x)  =  g x


-- fst . (f || g)         =  f . fst

-- fst . (f ^ g)          =  f

-- snd . (f || g)         =  g . snd

-- snd . (f ^ g)          =  g

-- (fst . h) ^ (snd . h)  =  h

-- (fst *** snd) . dup    =  id

-- (f || g) . (h ^ j)     =  (f . h) ^ (g . j)

-- (f ^ g) . h            =  (f . h) ^ (g . h)

-- (f || g) = (h || j) ==> f = h AND g = j

-- (f *** g) . dup = (h *** j) . dup ==> f = h AND g = j



-- (f ?|? g) . Left       =  Left . f

-- (f `v` g) . Left       =  f

-- (f ?|? g) . Right      =  Right . g

-- (f `v` g) . Right      =  g

-- (h . Left) `v` (h . Right)  =  h
--   explanation:  `v` untags, then the same tag is readded

-- Left `v` Right         =  id

-- (f `v` g) . (h ?|? j)  =  (f . h) `v` (g . j)

-- f . (g `v` h)          =  (f . g) `v` (f . h)

-- f ?|? g = h ?|? j ==> f = h AND g = j

-- f `v` g = h `v` j ==> f = h AND g = j



-- (f ^ g) `v` (h ^ j)       =  (f `v` h) ^ (g `v` j)
hmm :: (a -> b) -> (a -> d) -> (c -> b) -> (c -> d) -> Either a c -> (b, d)
hmm f g h j = ((f *** g) . dup) `v` ((h *** j) . dup)


pQ :: (a -> Bool) -> a -> Either a a
pQ p x 
  | p x         =  Left x
  | otherwise   =  Right x

-- (f `v` g) . (p? p)  ==>   if p then f else g

void :: a -> ()
void = const ()
