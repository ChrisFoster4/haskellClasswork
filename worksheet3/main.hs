module Main where
import Prelude hiding ((&&))

--Pattern Matching Exercises
--Exercise 1
(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _ _ = False

--Exercise 2 
--XOR using pattern matching
exOr :: Bool -> Bool -> Bool
exOr True False = True
exOr False True = True
exOr _ _ = False

--Exercise 3
--Define if then else using pattern matching
--ifThenElse :: Bool -> Int -> Int -> Int --If bool is true then second argument else give third arguemnt
--ifThenElse True = 

--Exercise 4a
-- Takes an int between 1 and 12 returns how many days are in the month. Assumes non leap year
daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4  = 30   
daysInMonth 6 =30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth _ = 31 --Months(1,3,5,7,8,10,12)

--Exercise 4b
--TODO Similar validDate(from previous worksheet)
--Checking if day enetered is a day within the month entered
validDate :: Int -> Int -> Bool
validDate 31 1 = True --TODO make this actually work
validDate _ _= False

--Recusion Exercises
--Exercise 5
--Like a factorial but with + instead of *
sumNumber :: Int -> Int
sumNumber 1 = 1
sumNumber n = n + sumNumber(n-1) 

--Exercise 6
--Sum of first n squares where n is the argument
sumSquares :: Int -> Int
sumSquares 1 = 1
sumSquares n = n^2 + sumSquares(n-1)

--Exercise 7
--Raise the first argument to the power of the second argument
power :: Int -> Int -> Int --TODO make this recurrsive
power b n = b^n 


--Exercise 8
--Gives the product of all integers between the two arguments
sumFromToProper :: Int -> Int -> Int
sumFromToProper arg1 arg2 = sum [arg1..arg2] --TODO write this recurrsivly

--sumFromTo :: Int -> Int -> Int
--sumFromTo arg1<arg2 arg2 = 5

--Exercise 9


--Exercise 10
--Exercise 11 
--Where guards have been used use pattern matching and vice versa
--Exercise 5 Version 2
--Exercise 6 Version 2
--Exercise 7 Version 2
--Exercise 8 Version 2
--Exercise 9 Version 2
--Exercise 10 Version 2


main = do
	--print $ True && True
	--print $ False && True
	--print $ exOr False True
	--print $ exOr True True
	--print $ exOr False False
	--print $ sumNumber 3
	--print $ sumNumber 4
	--print $ sumSquares 3
	--print $ power 2 3
	print $ sumFromTo 5 8
	putStrLn "EOP"
	
