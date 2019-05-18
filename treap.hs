module Treap where

data Treap a b = Empty | Treap (Treap a b) (a, b) (Treap a b)

split :: Ord a => Treap a b -> a -> (Treap a b, Treap a b)

split Empty a = (Empty, Empty)
split (Treap left (key, prior) right) a | key < a =
    let (splitL, splitR) = split right a 
        in (Treap left (key, prior) splitL, splitR)
                                        | otherwise =
    let (splitL, splitR) = split left a 
        in (splitL, Treap splitR (key, prior) right)

merge :: Ord b => Treap a b -> Treap a b -> Treap a b

merge Empty b = b
merge a Empty = a
merge (Treap leftL (al, bl) rightL) (Treap leftR (ar, br) rightR) | bl < br =
    Treap leftL (al, bl) (merge rightL (Treap leftR (ar, br) rightR))
                                                                  | otherwise =
    Treap (merge (Treap leftL (al, bl) rightL) leftR) (ar, br) rightR



