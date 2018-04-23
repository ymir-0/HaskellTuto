-- parse a number to digits
toDigits :: Integer -> [Integer]
toDigits number
 | number <= 0 = []
 | otherwise = toDigits (div number 10) ++ [mod number 10]
-- reverse a list
toDigitsRev :: Integer -> [Integer]
toDigitsRev number
 | number <= 0 = []
 | otherwise = mod number 10 : toDigitsRev (div number 10)
-- reverse array
reverseArray :: [Integer] -> [Integer]
reverseArray [] = []
reverseArray (element:[]) = [element]
reverseArray (element:leftover) = reverseArray(leftover)++[element]
-- double every 2nd number from right to left
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (element:[]) = element:[]
doubleEveryOther (element1:element0:[]) = element1:2*element0:[]
doubleEveryOther (element1:element0:leftover) = element1:2*element0:doubleEveryOther(reverseArray(leftover))
-- sum digits
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (element:[]) = sum (toDigits element)
sumDigits (element:leftover) = sumDigits([element])+sumDigits(leftover)
