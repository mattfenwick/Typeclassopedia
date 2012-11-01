module Datums (

    BinTree (..)
    
  , Product (Product)
  , unProduct
  
  , Id (..)
  
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