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
hanoi 1 a b c = [(a,b)]
hanoi n a b c = ((hanoi (n-1) a c b)++[(a,b)])++(hanoi (n-1) c b a)

reversePegs :: [Peg] -> [Peg]
reversePegs [] = []
reversePegs (element:[]) = [element]
reversePegs (element:leftover) = reversePegs(leftover)++[element]

getFirst :: [Peg] -> Peg
getFirst (element:_) = element

getLast :: [Peg] -> Peg
getLast (pegs) = getFirst(reversePegs(pegs))

removeFirst :: [Peg] -> [Peg]
removeFirst (peg:[]) = [peg]
removeFirst (peg:leftover) = leftover 

removeLast :: [Peg] -> [Peg]
removeLast pegs = reversePegs(removeFirst(reversePegs(pegs)))

removeFirstLast :: [Peg] -> [Peg]
removeFirstLast pegs = removeFirst(removeLast(pegs))

hanoiExt :: Integer -> Peg -> [Peg] -> Peg -> [Move]
hanoiExt 1 peg0 _ peg1 = [(peg0,peg1)]
hanoiExt n peg0 [] peg1 = [(peg0,peg1)]
hanoiExt n peg0 pegs peg1 = ((hanoiExt (n-1) (getFirst(pegs)) (removeFirstLast(pegs)) (getLast(pegs)))++[(peg0,peg1)])++ ( hanoiExt (n-1) (getFirst(pegs)) (peg0:removeFirst(pegs)) peg1 )
--hanoiExt n peg0 pegs peg1 = (hanoiExt (n-1) (getFirst(pegs)) (removeFirstLast(pegs)) (getLast(pegs)))++[(peg0,peg1)]
--hanoiExt n peg0 pegs peg1 = hanoiExt (n-1) (getFirst(pegs)) (removeFirstLast(pegs)) (getLast(pegs))
