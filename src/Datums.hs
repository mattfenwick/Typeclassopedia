module Datums (

    BinTree (..)
    
  , Product (Product)
  , unProduct
  
  , Id (..)

) where


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