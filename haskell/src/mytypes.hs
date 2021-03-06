
-- inbuilt types
-- :t gives the type of a var

--Functions also have types. 
--When writing our own functions, we can choose to give them an explicit type declaration. 
--This is generally considered to be good practice except when writing very short functions. 
--From here on, we'll give all the functions that we make type declarations. 

import Data.Char (toLower)
convertToLower :: [Char] -> [Char] 
convertToLower x = [ toLower c | c <- x ]

--Typeclasses 101
--A typeclass is a sort of interface that defines some behavior. 
--If a type is a part of a typeclass, that means that it supports and implements the behavior the typeclass describes. 
--A lot of people coming from OOP get confused by typeclasses because they think 
--they are like classes in object oriented languages. Well, they're not. 
--You can think of them kind of as Java interfaces, only better.
--ghci :t (+)
-- (+) :: Num a => a -> a -> a

-- User defined types
-- https://www.haskell.org/tutorial/goodies.html

data Color    = Red | Green | Blue | Indigo | Violet

print_color :: Color -> [Char]

print_color Red = "chumapu"
print_color Green = "pacha"
print_color Blue = "neela"
print_color Indigo = "koppu"
print_color Violet = "arinjuda"

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 

circle1 = Circle 0 0 4
area_circle1 = surface circle1

printMe :: Shape -> [Char]
printMe (Circle x y r) = "A circle with center "

--center_pt :: Shape -> (Float, Float)
