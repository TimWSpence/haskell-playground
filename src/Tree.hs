module Tree
  (
  )
where

data Tree a = Leaf | Node a (Tree a) (Tree a)

depth :: Tree a -> Int
depth Leaf = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

fold :: (b -> a -> b) -> b -> Tree a -> b
fold _ b Leaf = b
fold f b (Node a l r) = fold f b'' r
  where
    b' = fold f b l
    b'' = f b' a

foldMonoid :: Monoid m => Tree m -> m
foldMonoid = fold (<>) mempty
