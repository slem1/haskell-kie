module Polymorphism (
    elemInt
) where


elemInt :: Int -> [Int] -> Bool
elemInt elem [] = False
elemInt elem (x:xs) = if x == elem then True else elemInt elem xs 