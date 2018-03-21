module Main where

--Exercise 1
absolute :: Int -> Int
absolute number
		| number > 0  = number
		| otherwise  = (number * (-1))

--Exercise 2
sign :: Int -> Int
sign number | number == 0 = 0
	    | number > 0 = 1
	    | number < 0 = -1 --otherwise expression could be used here

--Exercise 3
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
	| x==y && x==z  = 3
	| x==y || x==z || y==z = 2
	| otherwise = 0

--Exercise 4
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths side1 side2 side3 = (diagonalLength side1) + (diagonalLength side2) + (diagonalLength side3)
		where
		diagonalLength length = sqrt(length^2+length^2)--Implementation of Pythagorus

--Exercise 5
taxiFare :: Int -> Float
taxiFare distance | distance <= 10 = 2.2+ (fromIntegral(distance)*0.5)
		  | otherwise = 7.2+((fromIntegral(distance-10)) * 0.3)




--Exercise 6
validDate :: Int -> Int -> Bool
validDate day month
	| (month == 1|| month == 3 ||month == 5 || month == 7 || month == 8 || month ==10 || month == 12) && (day <= 31 && day >0) = True
	| (month == 4 || month == 6 || month == 9||month ==11) && (day <= 30 && day >0) = True
	| month == 2 && (day <= 28 && day > 0) = True
  | otherwise = False  --If a month more than 12 ect.

--Exercise 7
howManyAboveAverage :: Int -> Int -> Int -> Int --Takes three ints and returns the number of them above average
howManyAboveAverage num1 num2 num3
	| (num1 > average && num2 > average && num3 > average) = 3
	| (num1 > average && num2 > average || num1 > average && num3 > average) = 2
	| (num1 > average || num2 > average || num3 > average) = 1
	| otherwise = 0
	where
	average = (num1 + num2 + num3)`div`3

--Exercise 8
daysInAMonth :: Int -> Int -> Int --Input month and year return days in a month
daysInAMonth month year
	| (month == 1|| month == 3 ||month == 5 || month == 7 || month == 8 || month ==10 || month == 12) = 31
	| (month == 4 || month == 6 || month == 9||month ==11) = 30
	| month == 2 && ((year `mod` 4) == 0) = 29
	| month == 2 = 28 --Tests down the guard and stops testing once a condition is met

main = do --Tests
	print $ absolute 5
	print $ absolute (-5)
	print $ howManyEqual 5 5 5
	print $ howManyEqual 5 6 5
	print $ howManyEqual 5 6 6
	print $ howManyEqual 8 6 9
	print $ daysInAMonth 2 1000
	print $ daysInAMonth 2 1001
	putStrLn "EOP"
