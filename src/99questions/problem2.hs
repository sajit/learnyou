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
