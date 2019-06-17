data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = foldMap f l `mappend` f x `mappend` foldMap f r


testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )


//TODO: Skad foldl bierze Monoid'a zeby uzyc implementacjjij folmMap ????
test1 = foldl (+) 0 testTree
test2 = foldl (\a x -> a ++ (show x)) [] testTree
test3 = foldMap (\x -> [x]) testTree
test4 = foldl (\a x -> a ++ [x]) [] testTree


newtype InfixTree a = InfixTree { getTreeInfix :: Tree a } deriving (Eq, Read, Show)
instance Foldable InfixTree where
    foldMap f (InfixTree Empty) = mempty
    foldMap f (InfixTree (Node x l r)) = foldMap f (InfixTree l) `mappend` f x `mappend` foldMap f (InfixTree r)

newtype PrefixTree a = PrefixTree { getTreePrefix :: Tree a } deriving (Eq, Read, Show)
instance Foldable PrefixTree where
    foldMap f (PrefixTree Empty) = mempty
    foldMap f (PrefixTree (Node x l r)) = f x `mappend` foldMap f (PrefixTree l) `mappend` foldMap f (PrefixTree r)

newtype PostfixTree a = PostfixTree { getTreePostfix :: Tree a } deriving (Eq, Read, Show)
instance Foldable PostfixTree where
    foldMap f (PostfixTree Empty) = mempty
    foldMap f (PostfixTree (Node x l r)) = foldMap f (PostfixTree l) `mappend` foldMap f (PostfixTree r) `mappend` f x





{- MONADY -}
//TODO: jakbysmy zdefiniowali "landLeft :: Birds -> Pole -> Maybe Pole" jako "landLeft :: Pole -> Birds -> Maybe Pole" to bysmy nie uzyli (>>=) !!!!

//TODO:
-- UWAGA: Kolejność od lewej do prawej jest WAŻNA !!!! Nie jest tak jak w Monoidzie, że operacja jest łączna !!!!!!
let nothing = return (0,0) >>= landLeft 3 >>= landLeft 1 >>= landLeft (-1)
-- Jakbyśmy najpierw wykonali te operacje po prawej, to byśmy uzyskali Just (3,0) a poprawnym wynikiem jest Nothing !!!!!

















