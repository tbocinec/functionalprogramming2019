module MinMax where

data BTree t = Node (BTree t) t (BTree t) | Nil deriving(Show, Eq)
maxTree :: (Ord t) => (BTree t) -> t
maxTree (Node Nil t Nil) = t
maxTree (Node l  t Nil) = max (maxTree l)  t
maxTree (Node Nil t r) = max (maxTree r) t
maxTree (Node l  t r) = max (max (maxTree l)  (maxTree r)) t

minTree :: (Ord t) => (BTree t) -> t
minTree (Node Nil t Nil) = t
minTree (Node l  t Nil) = min (minTree l)  t
minTree (Node Nil t r) = min (minTree r) t
minTree (Node l  t r) = min (min (minTree l)  (minTree r)) t

minmax :: (Ord t) => (BTree t) -> (t, t)
minmax x = (minTree x,maxTree x)
