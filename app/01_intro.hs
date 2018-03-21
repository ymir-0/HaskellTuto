-- *****Declarations and variables
x :: Int
x = 3
--x = 4
--y :: Int
--y = y + 1 -- 'y' is defined recursively as "himself + 1" => infinite loop
-- *****Basic Types
i :: Int
i = -78
-- Machine-sized integers
biggestInt, smallestInt :: Int
biggestInt  = maxBound
smallestInt = minBound
-- Arbitrary-precision integers
n :: Integer
n = 1234567890987654321987340982334987349872349874534
reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))
numDigits :: Int
numDigits = length (show reallyBig)
-- Double-precision floating point
d1, d2 :: Double
d1 = 4.5387
d2 = 6.2831e-4
-- Booleans
b1, b2 :: Bool
b1 = True
b2 = False
-- Unicode characters
c1,c2,c3 :: Char
c1 = 'x'
c2 = 'Ø'
c3 = 'ダ'
-- Strings are lists of characters with special syntax
s :: String
s = "Hello, Haskell!"
-- *****GHCi
-- Arithmetic
ex01 = 3 + 2
ex02 = 19 - 27
ex03 = 2.35 * 8.6
ex04 = 8.7 / 3.1
ex05 = mod 19 3
ex06 = 19 `mod` 3 -- character ` make a function name into an infix operator
ex07 = 7 ^ 222
ex08 = (-3) * (-7) -- negative numbers must often be surrounded by parentheses, to avoid having the negation sign parsed as subtraction
--badArith1 = i + n -- Haskell does not do implicit conversion
ex09 = i `div` i
ex10 = 12 `div` 5
-- Boolean logic
ex11 = True && False
ex12 = not (False || True)
ex13 = ('a' == 'a')
ex14 = (16 /= 3)
ex15 = (5 > 3) && ('p' <= 'q')
ex16 = "Haskell" > "C++"
-- *****Defining basic functions
-- Compute the sum of the integers from 1 to n.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)
-- Conditional
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1
foo :: Integer -> Integer
foo 0 = 16
foo 1 
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3
-- is even ?
isEven :: Integer -> Bool
isEven n 
  | n `mod` 2 == 0 = True
  | otherwise      = False
-- *****Pairs
p :: (Int, Char)
p = (3, 'x')
sumPair :: (Int,Int) -> Int
sumPair (x,y) = x + y
-- *****Using functions, and multiple arguments
f :: Int -> Int -> Int -> Int
f x y z = x + y + z
ex17 = f 3 17 8
-- *****
-- *****RUN
--y,reallyBig
--main = print (x,i,biggestInt,smallestInt,n,numDigits,d1,d2,b1,b2,c1,c2,c3,s)
--main = print (ex01,ex02,ex03,ex04,ex05,ex06,ex07,ex08,ex09,ex10)
--main = print (ex11,ex12,ex13,ex14,ex15,ex16)
--main = print (map sumtorial [0,3], map hailstone [2,3] , map foo [(-3),0,1,36,38], map isEven [0,1,2,3])
main = print (p,sumPair(3,5),ex17,let n=4 in f 3 (n+1) 7)
