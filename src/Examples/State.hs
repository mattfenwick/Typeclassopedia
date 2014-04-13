module Examples.State (

) where

import Datums (State(..), set, fetch, BinTree(..), showBinTree)
import Classes
import Instances
import Prelude hiding ((>>=), (>>))


updateState :: (s -> s) -> State s s
updateState f = 
    fetch >>= \s1 ->
    let s2 = f s1 in set s2 >> fetch

number :: BinTree a -> State Int (BinTree Int)
number (Leaf _) = fetch >>= \n -> State (\s -> (s + 1, Leaf n))
number (Node x y) = 
    number x >>= \t1 ->
    number y >>= \t2 ->
    pure (Node t1 t2)

example :: BinTree Char
example = Node (Leaf 'x')
               (Node (Node (Leaf 'y') (Leaf 'z'))
                     (Node (Node (Leaf 'q') (Leaf 'r'))
                           (Leaf 's')))

runExample = putStrLn . showBinTree . snd $ (getState $ number example) 1

