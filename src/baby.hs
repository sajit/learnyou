doubleMe x = 2 * x

tripleMe y = y + y + y

sqr a = a * a

sumSquare x y = sqr x + sqr y + 2 * x * y

--else is mandatory in haskell
doubleSmallNumber n = if n < 100 then 2*n else n

--aliasing
foo x = bar x
bar x = x + 1
