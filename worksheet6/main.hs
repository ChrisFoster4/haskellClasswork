import qualified Data.Char -- Used in Exercise 2 and 13


--Exercise 1
mult10 :: [Int] -> [Int]
mult10 = map (*10)

--Exercise 2
--onlyLowerCase :: [Char] -> [Char]
onlyLowerCase = filter Data.Char.isLower

--Exercise 3
orAllAlt :: [Bool] -> Bool
orAllAlt = elem True

orAll :: [Bool] -> Bool
orAll = foldr (||) False

--Exercise 4
sumSquares :: [Int] -> Int
sumSquares list = sum $ map (^2) list

sumSquaresAlt :: [Int] -> Int
sumSquaresAlt  = foldr (+) 0  . map (^2)

--Exercise 5
zeroToTen list = filter (<= 10) $ filter (>=0) list
--Exercise 6
squareRoots = filter (>0) . map sqrt

--Exercise 7
--Has to be recurrisive?
countBetween upper lower list = length $ filter (\x -> x >= upper && x <= lower) list

countBetweenAlt upper lower list = length $ filter (>=upper) $ filter (<= lower) list

countBetweenAltAlt upper lower list = foldr (+) 0 . filter (>=upper) . filter (<= lower) list

--Exercise 8
alwaysPositive function list = ( filter (<0) $  map function list ) == []

alwaysPositiveAlt function list = 0 == (length $ map function list)
-- TODO make this work with two maps and a filter

alwaysPositiveAltAlt function = foldr (+) 0 .  map (^0) . map function

--Exercise 9
productSquareRoots list = product $ squareRoots list

productSquareRootsAlt list = foldr (*) 1 $ squareRoots list

--Other higher order function exercises
--Exercise 10
--Remove the first element that meets a criteria
removeFirst _ [] = [] --What higher order function am I meant to use here
removeFirst toRemove (x:xs) = if toRemove == x then xs else x : removeFirst toRemove xs

--Exercise 11
removeLast toRemove list = removeFirst toRemove (reverse list) --Is this the intended way?

--Lambda exercises
--Exercise 12
zeroToTenAlt list = filter (\x -> x <= 10 &&  x >= 0) list

--Exercise 13
--Using only Lamdas and foldr's
--Part 1 - mult10
mult10Alt list = foldr (\ head' tail' -> (head' * 10) : tail') [] list

mult10AltAlt [] = []
mult10AltAlt (x:xs) = x*10 : mult10Alt xs

--Part 2 - reverse a list
myReverse list = foldl (\ head' tail' -> tail' : head') [] list

myReverseAlt (x:xs) = myReverse xs ++ [x]
myReverseAlt [] = []

--Part 3 - onlyLowerCase
onlyLowerCaseAlt list = foldr (\ head' tail' -> if Data.Char.isLower head' then head' : tail' else tail') [] list

onlyLowerCaseAltAlt [] = []
onlyLowerCaseAltAlt (x:xs) = if Data.Char.isLower x then x : onlyLowerCase xs else onlyLowerCase xs
