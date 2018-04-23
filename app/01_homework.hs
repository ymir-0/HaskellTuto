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
-- double every 2nd number from LEFT to RIGHT
doubleEveryStd :: [Integer] -> [Integer]
doubleEveryStd [] = []
doubleEveryStd (element:[]) = element:[]
doubleEveryStd (element1:element0:leftover) = element1:2*element0:doubleEveryStd(leftover)
-- double every 2nd number from RIGHT to LEFT
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (array) = reverseArray(doubleEveryStd(reverseArray(array)))
-- sum digits
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (element:[]) = sum (toDigits element)
sumDigits (element:leftover) = sumDigits([element])+sumDigits(leftover)
 -- validate credit card
validate :: Integer -> Bool
validate(creditCardNumber) = (mod (sumDigits(doubleEveryOther(toDigits(creditCardNumber)))) 10)==0
