module Bananas (

) where


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








