module Polymorphism (
    elemInt, printEqResult
) where


elemInt :: Int -> [Int] -> Bool
elemInt elem [] = False
elemInt elem (x:xs) = if x == elem then True else elemInt elem xs 

data IntTree = Leaf Int | Tree Int IntTree IntTree

instance Eq IntTree where 
    Leaf x == Leaf x' = x == x'
    Tree x left right == Tree x' left' right' = x == x' && left == left' && right == right'
    _ == _ = False 

data IntList = Nil | Cons Int IntList

instance Eq IntList where
    Nil == Nil = True
    Cons x tail == Cons x' tail' = x == x' && tail == tail'
    _ == _ = False

value1 :: IntList
value1 = Cons 3 (Cons 10 Nil)

value2 :: IntList
value2 = Nil

printEqResult = print (value1 == value1, value2 == value2, not (value1 == value2))