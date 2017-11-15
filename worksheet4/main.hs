import qualified Data.Char --Used for exercise 6

--Tuples Exercises

--Exercise 1
--Return the sum and the difference of two values
type TwoInts = (Int,Int)

sumDifference :: Int -> Int -> TwoInts
sumDifference num1 num2 = ((num1+num2),(num1-num2))

--Exercise 2
--Return student grade from percentage
type StudentMark = (String,Int)

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
onlyDigits word = [i | i <- word , Data.Char.isDigit i == True]
--onlyDigits word = filter Data.Char.isDigit word

--Exercise 8
capMarks :: [StudentMark] -> [StudentMark]
capMarks list = [capMark(i,j) | (i,j) <- list]

--Exercise 9
--gradeStudents :: [StudentMark] -> [(String,Char)]
--gradeStudents list = [(i,(grade j)) | (i,j) <- list] --TODO

--Exercise 10
nonRecursiveDuplicate :: Int -> String -> String
nonRecursiveDuplicate num string = concat $ replicate num string

--Exercise 11
--Exercise 12
--Exercise 13


main = do
	print $ capMarks [("Test0",50),("Test1",30)]
	--print $ gradeStudents [("Test0",50),("Test1",30)]
