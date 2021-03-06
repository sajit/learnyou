--problem2
--(*) Find the last but one element of a list.
module Problem2(lastb1_el) where

--import Problem1

lastb1_el aList = if(length(aList) >= 2) then Just (aList !! (length(aList)-2)) else Nothing

-- problem3 
--Find the K'th element of a list. 

kth_el aList  k = if(length(aList) < k) then Nothing else Just (aList !! (k-1))

-- problem 4 
-- Find the number of elements in a list
num_elements aList = length(aList)

recursive_kth :: [Integer] -> Integer -> Integer
recursive_kth [] k = -1
recursive_kth aList 0 = (head aList)
recursive_kth aList k = recursive_kth  (tail aList) (k-1) 

--reverse a list
reverse_list [] = []
reverse_list (x:xs) = reverse_list xs ++ [x]

-- Find out whether a list is a palindrome.
is_palindrome [] = True
is_palindrome [x] = True
is_palindrome (x:xs) = x == last xs && is_palindrome (init xs)

--Flatten a nested list structure.
-- define a data structure to accommodate nested lists since haskell lists are homogeneous
data NestedList a = Elem a | List [NestedList a]
--flatten

--Problem 8 
--(**) Eliminate consecutive duplicates of list elements.
distinct_neighbors [] = []
distinct_neighbors [x] = [x]
distinct_neighbors (x:xs) 
	| x == head(xs) = distinct_neighbors(xs)
	| otherwise = x : distinct_neighbors(xs)

-- Problem 9
--(**) Pack consecutive duplicates of list elements into sublists. 
--If a list contains repeated elements they should be placed in separate sublists.

pack_up [] (prev,count) acc =  reverse (init ([prev | x <- [1..count]] : acc))
pack_up (x:xs) (prev,count) acc 
 | x == prev = pack_up xs (prev,count+1) acc
 | otherwise = pack_up xs (x,1) ([prev | x <- [1..count]] : acc)

 --(*) Run-length encoding of a list. 
 --Use the result of problem P09 to implement the so-called run-length encoding data compression method. 
 --Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

encode_me :: [[Integer]] ->[(Int,Integer)] -> [(Int,Integer)]
encode_me [] acc = reverse acc
encode_me (x:xs) acc =  encode_me xs ((length x,head x) : acc) 


--(*) Modified run-length encoding.

--Modify the result of problem 10 in such a way that if an element has no duplicates
-- it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
data AnythingGoes = SingleMe Int | DoubleUs Int Int
filter_encode_0 [] = []
filter_encode_0 (x:xs)
 | fst x == 1  = SingleMe (snd x)  : filter_encode_0 xs
 | otherwise = DoubleUs (fst x) (snd x) : filter_encode_0 xs

--Modify run-length encoding part 2

-- Filter out encodings that have 1 length
filter_encode [] = []
filter_encode (x:xs)   
 | fst x == 1  = filter_encode xs
 | otherwise = x : filter_encode xs

print_SingleMe (SingleMe x) = x
print_DoubleUs (DoubleUs x y) = y

--(**) Decode a run-length encoded list.

decode_me [] = []
decode_me (x:xs) = [(snd x) | y <- [1.. (fst x)] ] ++ decode_me xs

--4 Problem 14
--(*) Duplicate the elements of a list.

duplicate_me [] = []
duplicate_me (x:xs) = x : x : duplicate_me xs
