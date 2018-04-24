-- ***** Validating Credit Card Numbers *****
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
-- ***** The Towers of Hanoi *****
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a _ c = [(a,c)]
hanoi n a b c = (hanoi (n-1) a c b)++[(a,c)]++(hanoi (n-1) b a c)

pegsLength :: [Peg] -> Integer
pegsLength [] = 0
pegsLength (_:leftover) = 1 + (pegsLength leftover)

reversePegs :: [Peg] -> [Peg]
reversePegs [] = []
reversePegs (peg:[]) = [peg]
reversePegs (peg:leftover) = reversePegs(leftover)++[peg]

getFirst :: [Peg] -> Peg
getFirst (peg:_) = peg

getLast :: [Peg] -> Peg
getLast pegs = getFirst(reversePegs(pegs))

removeFirst :: [Peg] -> [Peg]
removeFirst [] = []
removeFirst (_:leftover) = leftover

removeLast :: [Peg] -> [Peg]
removeLast (pegs) = reversePegs(removeFirst(reversePegs(pegs)))

dispatch :: Integer -> Peg -> [Peg] -> [Move]
dispatch 0 _ __ = []
dispatch n _ [] = []
dispatch n peg pegs = [(peg,getFirst(pegs))]++(dispatch (n-1) peg (removeFirst(pegs)))

regroup :: [Peg] -> Peg -> [Move]
regroup [] _ = []
regroup pegs peg = [((getFirst(pegs)),peg)]++(regroup (removeFirst(pegs)) peg)

hanoiExt :: Integer -> Peg -> [Peg] -> Peg -> [Move]
hanoiExt n peg0 pegs peg1
 | n <= 0 = []
 | n == 1 = [(peg0,peg1)]
 | pegs == [] = [(peg0,peg1)]
 | otherwise = (hanoiExt (n-pegsLength(pegs)) peg0 (removeFirst(pegs)) (getFirst(pegs)))++(dispatch (pegsLength(pegs)) peg0 ((removeFirst(pegs))++[peg1]))++(regroup (removeFirst(pegs)) peg1)++(hanoiExt (n-pegsLength(pegs)) (getFirst(pegs)) (peg0:(removeFirst(pegs))) peg1)
