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


whereis n 
  | charname == "Waldo" = "found"
  | otherwise = "not found"
  where charname = n  



maxValList [] =  error  "no max for empty list"
maxValList [x] = x
maxValList (x:xs) = max x  (maxValList xs)

--replicate :: (Num i,Ord i) => i -> a -> [a]
--replicate is a used function name..Rename it
--replcte n x 
--  | n <=0 = []
--  | otherwise = x:replcte (n-1) x
--Max With X, curried function
maxWithX x  = max x
--Max with 4 , curried function
maxWith4 = max 4 

-- lazy evaluation

bad_list = [1,5,65,error "oops",7]

-- this is ok
ok_head = head bad_list 

-- this is also ok
prefix_list = take 3 bad_list -- ok

suffix = drop 4 bad_list -- also ok
 
