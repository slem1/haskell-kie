module FoldIt (
    foldl',
    foldr'
) where


foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs  


foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)  

--foldInt :: [Int] -> [Int]
--foldInt lst = foldl' (++) [] lst 