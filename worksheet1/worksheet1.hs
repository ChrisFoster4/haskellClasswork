module Main where

timesTen :: Int -> Int
timesTen x = x * 10

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

areaOfCircle :: Float -> Float
areaOfCircle radius = (pi * radius)^2--pi r **2 Can use "**" or "^"

volumeOfCyclinder :: Float -> Float -> Float
volumeOfCyclinder height radius = height *  ( areaOfCircle radius)

distance :: Float -> Float -> Float -> Float -> Float
distance x1 x2 y1 y2 = sqrt $ ((y1 -y2)**2 + (x1-x2)**2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent num1 num2 num3 = ((num1/=num2)&&(num1/=num3))&&(num1/=num3)

{-
threeDifferent num1 num2 num3 = if (num1<num2<num3 || num1>num2>num3) then True else False
threeDifferent num1 num2 num3 = if num1 /= num2 && num1 /= num3 && num2 /= num1 && num2 /= num3 then True else False
Is there a better way?

-}

divisibleBy :: Int -> Int -> Bool
divisibleBy num1 num2 = (num1 `mod` num2) == 0

isEven :: Int -> Bool
isEven number = divisibleBy 2 number --same as "even number"

averageThree :: Int -> Int -> Int -> Float
averageThree num1 num2 num3 = fromIntegral(sumThree num1 num2 num3)/3

absolute :: Int -> Int --If the number is positive it returns the number.If the number is negative it returns the number without a minus sign
--absolute (number) = if (number) >= 0 then number else 0
--absolute (number) = if (number) >= 0 then number else (\number -> read $ tail $ show number :: Int) number
absolute (number) = if (number) >= 0 then number else number * (-1)


main :: IO()
main = do
	putStrLn "timesTen 5 called"
	print $ timesTen 5
	putStrLn "sumThree 5 7 13 called"
	print $ sumThree 5 7 13
	putStrLn "areaOfCircle 5.0 called"
	print $ areaOfCircle 5.0
	putStrLn "volumeOfCyclinder 5.0 10.0 called"
	print $ volumeOfCyclinder 5.0 10.0
	putStrLn "distance 5.0 20.0 70.0 90.0 called"
	print $ distance 5.0 20.0 70.0 90.0
	putStrLn "threeDifferent 5 6 7 called"
	print $ threeDifferent 5 6 7
	putStrLn "threeDifferent 5 5 7 called"
	print $ threeDifferent 5 5 7
	putStrLn "divisibleBy 5 10 called"
	print $ divisibleBy 5 10
	putStrLn "isEven 7 called"
	print $ isEven 7
	putStrLn "isEven 8 called"
	print $ isEven 8
	putStrLn "averageThree 1 5 7 called"
	print $ averageThree 1 5 7
	putStrLn "absolute 15 called"
	print $ absolute 15
	putStrLn "absolute 25 called"
	print $ absolute 25
	putStrLn "absolute 0 called"
	print $ absolute 0
	putStrLn "absolute (-10) called"
	print $ absolute (-10) --Returns error
	putStrLn "absolute 5 called"
	print $ absolute 5
