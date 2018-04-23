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
