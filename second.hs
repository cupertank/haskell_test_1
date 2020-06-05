import Control.Monad

data BSTree a = Nil 
              | Node {val :: a, left :: BSTree a, right :: BSTree a}
                deriving Show

tree =  Node 5 (Node 2 Nil (Node 10 Nil Nil)) Nil

findElem tree = do
    elem <- findElem' tree (Nothing, Nothing)
    print elem

findElem' Nil _ = return Nothing
findElem' (Node val left right) (parent, praparent) = do
    lans <- findElem' left (Just val, parent)
    rans <- findElem' right (Just val, parent)
    curans <- case (parent, praparent) of
              (Just x, Just y) -> if x * y == val then return (Just val) else return Nothing
              _                -> return Nothing
    return (curans `mplus` lans `mplus` rans)