module Polymorphism (
    elemInt, 
    printEqResult,
    List(..)
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

data List a = Empty | ConsLst a (List a)

instance Show a => Show (List a) where    
    show xs = "{" ++ show_ xs  ++ "}"

show_ :: Show a => (List a) -> String
show_ Empty = ""
show_ (ConsLst x Empty) = show x
show_ (ConsLst x xs) = show x ++ "," ++ show_ xs

    


