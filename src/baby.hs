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
