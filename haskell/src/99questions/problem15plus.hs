--5 Problem 15
--(**) Replicate the elements of a list a given number of times.

replicate_me [] n = []
replicate_me (x:xs) n = [x | y <- [1..n]] ++ replicate_me xs n