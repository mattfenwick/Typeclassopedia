module Datums (

    BinTree (..)
  , showBinTree
  
  , Id (..)
    
  , Product (Product)
  , unProduct
  
  , Seq (..)
  
  , Tree (Tree)
  , root
  , branches
  , showTree

) where


import Data.List (intersperse)


data BinTree a
    = Leaf a
    | Node (BinTree a) (BinTree a)
  deriving (Show, Eq, Ord)
  
  
  
data Id a 
    = Id a 
  deriving (Show, Eq, Ord)



newtype Product a 
    = Product {unProduct :: a} 
  deriving (Show, Eq, Ord)
  
  
  
data Seq a
    = End a
    | Cons a (Seq a)
  deriving (Show, Eq, Ord)
  
  

data Tree a = Tree {
      root     :: a,
      branches :: [Tree a]
  } deriving (Show, Eq, Ord)
  
  
  
showTree :: Show a => Tree a -> String
showTree t = help t 0
  where 
    help :: (Show b) => Tree b -> Int -> String
    help (Tree x bs) n = concat $ intersperse "\n" (show x : map (branch n) bs)
    branch n br = replicate n ' ' ++ "- " ++ help br (n + 1)
  
showBinTree :: Show a => BinTree a -> String
showBinTree t = help 0 t
  where 
    help :: Show b => Int -> BinTree b -> String
    help n (Leaf x)    = replicate n ' ' ++ show x
    help n (Node l r)  = concat $ intersperse "\n" ((replicate n ' ' ++ "--") : map (help (n + 1)) [l, r])
