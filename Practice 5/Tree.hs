module Tree where

 import Data.List

 -- Algebraic data type to build BinaryTree.

 data BinaryTree a = Void | Node (BinaryTree a) a (BinaryTree a) deriving (Show,Eq,Ord)

 --Exercise 2.1:

{-|
  Function that adds an element to an ordered binary tree, preserving order.
  e.g.
  >>>  addTree (Node (Node Void 1 Void) 2 (Node Void 3 Void)) 4
  Node (Node Void 1 Void) 2 (Node Void 3 (Node Void 4 Void) 
-}
 addTree :: (Ord a) => BinaryTree a -> a -> BinaryTree a
 addTree x y = case x of Void -> Node Void y Void
                         Node i c d -> case (y >= c) of True -> Node i c (addTree d y); otherwise -> Node (addTree i y) c d

 -- Exercise 2.2:

{-|
  Function that receives a BinaryTree and returns a list with the elements of the tree using inorder algorithm.
  e.g.
  >>>  inorder (Node (Node (Node Void 1 Void) 2 (Node Void 3 Void)) 4 (Node (Node Void 5 Void) 6 (Node Void 7 Void)))
  [1,2,3,4,5,6,7]
-}
 inorder :: BinaryTree a -> [a]
 inorder n = case n of Void -> []
                       Node Void c Void -> [c]
                       Node i c d -> inorder i ++ [c] ++ inorder d

 -- Exercise 2.3:

{-|
  Function that receives a BinaryTree and returns a list with the elements of the tree using preorder algorithm.
  e.g.
  >>>  preorder (Node (Node (Node Void 1 Void) 2 (Node Void 3 Void)) 4 (Node (Node Void 5 Void) 6 (Node Void 7 Void)))
  [4,2,1,3,6,5,7]
-}
 preorder :: BinaryTree a -> [a]
 preorder n = case n of Void -> []
                        Node Void c Void -> [c]
                        Node i c d -> [c] ++ preorder i ++ preorder d

 -- Exercise 2.4:

{-|
  Function that receives a BinaryTree and returns a list with the elements of the tree using postorder algorithm.
  e.g.
  >>>  postorder (Node (Node (Node Void 1 Void) 2 (Node Void 3 Void)) 4 (Node (Node Void 5 Void) 6 (Node Void 7 Void)))
  [1,3,2,5,7,6,4]
-}
 postorder :: BinaryTree a -> [a]
 postorder n = case n of Void -> []
                         Node Void c Void -> [c]
                         Node i c d -> postorder i ++ postorder d ++ [c]

 -- Exercise 2.5:

{-|
  Function that returns the greater element of a BinaryTree ordered.
  e.g.
  >>>  maximo (Node (Node Void 1 Void) 2 (Node Void 3 Void))
  3
-}
 maximo :: (Ord a) => BinaryTree a -> a
 maximo n = head $ reverse $ sort $ inorder n

 -- Exercise 2.6:

{-|
  Function that returns the smallest element of a BinaryTree ordered.
  e.g.
  >>>  minimo (Node (Node Void 1 Void) 2 (Node Void 3 Void))
  1
-}
 minimo :: (Ord a) => BinaryTree a -> a
 minimo n = head $ sort $ inorder n

 -- Exercise 2.7:

{-|
  Function that receives an element and an ordered BinaryTree and tells us if the element belongs to the BinaryTree or not.
  e.g.
  >>>  busca 3 (Node (Node Void 1 Void) 2 (Node Void 3 Void))
  True
-}
 busca :: (Ord a) => a -> BinaryTree a -> Bool
 busca n a = case a of Void -> False
                       Node Void c Void -> c == n
                       Node i c d -> busca n i || busca n d || n == c

 --Extra:

 -- Exercise 3.2:

{-|
  Function that receives a function, a BinaryTree and applies the function to each element of the tree.
  e.g.
  >>>  mapTree even (Node (Node Void 1 Void) 2 (Node Void 3 Void))
  Node (Node Void False Void) True (Node Void False Void)
-}
 mapTree :: ( a -> b ) -> BinaryTree a -> BinaryTree b
 mapTree f a = case a of Void -> Void
                         Node Void x Void -> Node Void (f x) Void
                         Node i c d -> Node (mapTree f i) (f c) (mapTree f d)