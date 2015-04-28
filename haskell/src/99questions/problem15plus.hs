--5 Problem 15
--(**) Replicate the elements of a list a given number of times.

replicate_me [] n = []
replicate_me (x:xs) n = [x | y <- [1..n]] ++ replicate_me xs n

--Problem 16
--(**) Drop every N'th element from a list.

dropNth [] n acc = acc
dropNth x n acc 
 | (length x) < n = x ++ acc
 | (length x) == n = (take (n-1) x) ++ acc
 | otherwise =  dropNth (drop n x) n ((take (n-1) x) ++ acc)  


--7 Problem 17
--(*) Split a list into two parts; the length of the first part is given.

split_list x n = ((take n x) , (drop n x))

--(**) Extract a slice from a list.
--Given two indices, i and k, the slice is the list containing the elements between 
--the i'th and k'th element of the original list (both limits included). 

splice x i k  
  | k >= (length x) = error "bad argument"
  | otherwise =  drop (i-1) (take k x)