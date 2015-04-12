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