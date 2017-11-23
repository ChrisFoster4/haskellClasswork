import qualified Data.List --Used for exercise 13
import qualified Data.Char --Used for exercise 6

--Tuples Exercises

--Exercise 1
--Return the sum and the difference of two values
type TwoInts = (Int,Int)

sumDifference :: Int -> Int -> TwoInts
sumDifference num1 num2 = ((num1+num2),(num1-num2))

--Exercise 2
--Return student grade from percentage
type StudentMark = (String,Int) --Not sure if this is meant to be (String,Int) or Int

grade :: StudentMark -> Char
grade (_,mark)
		| mark >= 70 = 'A'
		| mark >= 60 = 'B'
		| mark >= 50 = 'C'
		| mark >= 40 = 'D'
		| otherwise = 'F'

--Exercise 3
--Return 40 or less
capMark :: StudentMark -> StudentMark
capMark (name,mark)
		| mark >= 40 = (name,40)
		| otherwise = (name,mark)

--List and String exercises

--Exercise 4
firstNumbers :: Int -> [Int]
firstNumbers number = [1..number]

--Exercise 5
firstSquares :: Int -> [Int]
firstSquares number = [ i^2 | i <- [1..number]]

--Exercise 6
capitalise :: String -> String
capitalise word = map Data.Char.toUpper word

--Exercise 7
onlyDigits :: String -> String
onlyDigits word = [i | i <- word , Data.Char.isDigit i]--If isDigit i == true

onlyDigitsAlt :: String -> String
onlyDigitsAlt word = filter Data.Char.isDigit word

--Exercise 8
capMarks :: [StudentMark] -> [StudentMark]
capMarks list = [capMark(i,j) | (i,j) <- list]

--Exercise 9
gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents list = [(i,(grade (i,j))) | (i,j) <- list]

--Exercise 10
nonRecursiveDuplicate :: Int -> String -> String
nonRecursiveDuplicate num string = concat $ replicate num string

duplicate :: Int -> String -> String
duplicate numOfTimes string = recurviseGuard string numOfTimes

recurviseGuard :: String -> Int -> String
recurviseGuard string numOfTimes
		| numOfTimes == 1 = string
		| otherwise = string ++ (recurviseGuard string (numOfTimes - 1))

--Exercise 11
divisors :: Int -> [Int]
divisors number = [i | i <- [1..number],number `mod` i == 0]

--Exercise 12
isPrime :: Int -> Bool
isPrime number =  (length $ divisors number) <= 2

--Exercise 13
splitMapVer :: [(a,b)] -> ([a],[b])
splitMapVer input = (fst `map` input,snd `map` input) --TODO use list comprehension not map

splitUnzipVer :: [(a,b)] -> ([a],[b])
splitUnzipVer list = Data.List.unzip list


--Can you return two lists from a list comprehension
--Could this be done with a custom data type
--Expected output : ([1,2,3],"abc") from [(1,'a'),(2,'b'),(3,'c')]
split :: [(a,b)] -> ([a],[b])
split list = ([a|(a,_) <- list],[b|(_,b) <- list])


main = do
	print $ splitMapVer [(1,'a'),(2,'b'),(3,'c')]
	print $ splitUnzipVer [(1,'a'),(2,'b'),(3,'c')]
	print $ split [(1,'a'),(2,'b'),(3,'c')]
	print $ gradeStudents [("Jo",47), ("Sam",76)]
