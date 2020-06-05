import Control.Monad

data BSTree a = Nil 
              | Node {val :: a, left :: BSTree a, right :: BSTree a}
                deriving Show

instance Foldable BSTree where
    foldr f acc Nil                   = acc
    foldr f acc (Node val left right) = do
        foldr f (f val acc) left
        foldr f (f val acc) right
        f val acc


findElem tree = foldM findElem' (Nothing, Nothing) tree

findElem' (Nothing, Nothing) val = return (Nothing, Just val)
findElem' (Nothing, Just right)   val = return (Just val, Just right)

findElem' (Just parent, Just praparent) val = do
    if parent * praparent == val then
        print val
    else
        print ""
    return (Just val, Just parent)

tree =  Node 5 (Node 2 Nil (Node 10 Nil Nil)) Nil