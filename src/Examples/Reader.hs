module Examples.Reader (

) where

import Datums
import Classes
import Instances
import Prelude hiding ((>>=), fmap)


eg1 = 
    pure (,,)            <*>
    Reader ("butt" :) <*>
    ask                  <*>
    local ("stupid" :) ask

example :: BinTree Char
example = Node (Leaf 'x')
               (Node (Node (Leaf 'y') (Leaf 'z'))
                     (Node (Node (Leaf 'q') (Leaf 'r'))
                           (Leaf 's')))

-- depth :: BinTree a -> BinTree Int
depth (Leaf _) = ask >>= \d -> pure (Leaf d)
depth (Node x y) = 
    local (+ 1) (depth x) >>= \l ->
    local (+ 1) (depth y) >>= \r ->
    pure (Node l r)

runDepth d = 
    putStrLn (showBinTree example) >>= \_ ->
    putStrLn (showBinTree (getReader (depth example) d))

exaTree :: MyTree Char
exaTree = Branch 'x' 
               Empty
               (Branch 'y' 
                   (Branch 'q'
                       (Branch 'r' (Branch 's' Empty Empty) Empty)
                       Empty)
                   (Branch 'z' Empty Empty))


path Empty = pure Empty
path (Branch c l r) = 
    ask >>= \p ->
    local (c :) (path l) >>= \lval ->
    local (c :) (path r) >>= \rval ->
    pure (Branch (c:p) lval rval)
    
runMy x = 
    putStrLn (showMyTree exaTree) >>= \_ ->
    putStrLn (showMyTree (getReader (path exaTree) x))

