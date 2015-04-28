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