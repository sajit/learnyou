--(*) Find the last element of a list. returns Nothing if empty list
-- things to learn, the Haskell Maybe Just Nothing paradigm

module Problem1(last_el) where

last_el aList = if(length(aList) > 0) then Just (aList !! (length(aList)-1)) else Nothing


--pattern match example
sum_up :: [Integer] -> Integer -> Integer
sum_up [] sum = sum
sum_up (x:xs) sum = sum_up xs (sum+x) 

--guarded example
rec_sum_up a_list sum = if(length(a_list)==0) 
							then return sum 
							else return rec_sum_up tail a_list (sum + head a_list) 
   
foo al sum
    | length (al) == 0 = return sum
    | otherwise = return foo tail (al) (sum + head (al))