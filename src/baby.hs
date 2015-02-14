doubleMe x = 2 * x

tripleMe y = y + y + y

sqr a = a * a

sumSquare x y = sqr x + sqr y + 2 * x * y

--else is mandatory in haskell
doubleSmallNumber n = if n < 100 then 2*n else n

--aliasing
foo x = bar x
bar x = x + 1

--list comprehensions
-- get a list of double of all numbers between 1 and 10
evens = [x*2 | x <- [1..10]]
-- has a predicate, where x*2 <= 10
conditionalEvens = [x*2 | x <- [1..10],x*2 <=10]
--How about if we wanted all numbers from 50 to 100 whose remainder when divided with the number 7 is 3?
meh = [x | x <- [50..100], x `mod` 7 == 3]

-- :t gives you the type of a variable and :: indicates typeof
isEven x 
   | x `mod` 2 == 0 = "even"
   | otherwise = "odd"

-- pattern matchin for functions
luckyGiroo :: (Integral a) => a -> String
luckyGiroo 1  = "positive"
luckyGiroo 2  = "Twp"
luckyGiroo 3 = "Three"
luckyGiroo x  = "Dunno"

-- currying of functions
--mySum1 :: (Integer a ) => a -> a -> a
mySum1 x y = x + y
mySum2 x = mySum1 1 x
mySum3 = mySum1 1

tupleSum (x1,y1) (x2,y2) = (x1+x2,y1+y2)  
