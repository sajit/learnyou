--(*) Find the last element of a list. returns Nothing if empty list
-- things to learn, the Haskell Maybe Just Nothing paradigm

module Problem1(last_el) where

last_el aList = if(length(aList) > 0) then Just (aList !! (length(aList)-1)) else Nothing