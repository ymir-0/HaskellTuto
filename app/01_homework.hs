-- parse a number to digits
toDigits :: Integer -> [Integer]
toDigits number
 | number <= 0 = []
 | otherwise = toDigits (div number 10) ++ [mod number 10]
-- reverse a list
--toDigitsRev :: [Integer] -> [Integer]
--toDigitsRev list
-- | [] = []
-- | element : [] = [element]
-- | element : leftover = toDigitsRev leftover ++ [element]
