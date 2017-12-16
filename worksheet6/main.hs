import qualified Data.Char -- Used in Exercise 2


--Exercise 1
mult10 :: [Int] -> [Int]
mult10 x = map (*10) x

--Exercise 2
onlyLowerCase :: [Char] -> [Char]
onlyLowerCase string = filter Data.Char.isLower string

--Exercise 3
orAll :: [Bool] -> Bool
orAll list = elem True list

--Exercise 4
sumSquares :: [Int] -> Int
sumSquares list = sum $ map (^2) list

sumSquaresAlt :: [Int] -> Int
sumSquaresAlt list = foldr (+) 0 $ map (^2) list

--Exercise 5
--What type signiture is this.GHCI says zeroToTen :: (Ord a, Num a) => [a] -> [a]
zeroToTen list = filter (<= 10) $ filter (>=0) list
--Exercise 6
squareRoots list = filter (>0) $ map sqrt list

--Exercise 7
--Has to be recurrisive?
countBetween upper lower list = length $ filter (\x -> x >= upper && x <= lower) list

--Exercise 8
alwaysPositive function list = ( filter (<0) $  map function list ) == []

--Exercise 9
productSquareRoots list = product $ squareRoots list
productSquareRootsAlt list = foldr (*) 1 $ squareRoots list --Why do I need a 1 here not a 0?

--Other higher order function exercises
--Exercise 10
--Remove the first element that meets a criteria
removeFirst _ [] = [] --What higher order function am I meant to use here
removeFirst toRemove (x:xs) = if toRemove x then xs else x : removeFirst toRemove xs

--Exercise 11
removeLast toRemove list = removeFirst toRemove (reverse list) --Is this the intended way?

--Lambda exercises
--Exercise 12
zeroToTenAlt list = filter (\x -> x <= 10 &&  x >= 0) list

--Exercise 13
--Using only Lamdas and foldr's
--Part 1 - mult10 
--Part 2 - reverse a list
--Part 3 - onlyLowerCase
