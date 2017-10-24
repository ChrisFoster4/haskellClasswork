module Main where

absolute :: Int -> Int
absolute number | number > 0  = number
		| otherwise  = (number * (-1))

sign :: Int -> Int
sign number | number == 0 = 0
	    | number > 0 = 1
	    | number < 0 = -1 --otherwise expression could be used here

minFareCheck :: Float -> Float
minFareCheck fare | fare < 2.2 = 2.2
		  | otherwise = fare

--taxiFare :: Int -> Float
--taxiFare distance | distance < 10 = minFareCheck (0.5 + (0.5*distance))
		    -- | otherwise = distance * 0.5  -- check val to mult by

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
	| x==y && x==z  = 3
	| x==y || x==z || y==z = 2
	| otherwise = 0

validDate :: Int -> Int -> Bool
validDate day month 
	| month == 1 && day <= 31 = True
	| month == 2 && day <= 28 = True
	| month == 3 && day <= 31 = True
	| month == 4 && day <= 30 = True
	| month == 5 && day <= 31 = True
	| month == 6 && day <= 30 = True
	| month == 7 && day <= 31 = True
	| month == 8 && day <= 31 = True
	| month == 9 && day <= 30 = True
	| month == 10 && day <= 31 = True
	| month == 11 && day <= 30 = True
	| month == 12 && day <= 31 = True
   	| otherwise = False  --If a month more than 12 ect.

howManyAboveAverage :: Int -> Int -> Int -> Int --Takes three ints and returns the number of them above average
howManyAboveAverage num1 num2 num3
	| (num1 > average && num2 > average && num3 > average) = 3
	| (num1 > average && num2 > average || num1 > average && num3 > average) = 2
	| (num1 > average || num2 > average || num3 > average) = 1
	| otherwise = 0
	where 
	average = (num1 + num2 + num3)`div`3 
	--average = fromIntegral(num1 + num2 + num3)/3 :: Float

daysInAMonth :: Int -> Int -> Int --Input month and year return days in a month
daysInAMonth month year 
	| month == 1 = 31
	| month == 2 && ((year `mod` 4) == 0) = 29
	| month == 2 = 28 --Tests down the guard and stops testing once a condition is met
	| month == 3 = 31
	| month == 4 = 30
	| month == 5 = 31
	| month == 6 = 30
	| month == 7 = 31 
	| month == 8 = 31
	| month == 9 = 30
	| month == 10 = 31
	| month == 11 = 30
	| month == 12 = 31 

main = do
	print $ absolute 5
	print $ absolute (-5)
	print $ howManyEqual 5 5 5
	print $ howManyEqual 5 6 5
	print $ howManyEqual 5 6 6
	print $ howManyEqual 8 6 9
	print $ daysInAMonth 2 1000
	print $ daysInAMonth 2 1001
	putStrLn "EOP"
