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

--grade :: StudentMark -> Char
--grade (_,mark)
--		| mark >= 70 = 'A'
--		| mark >= 60 = 'B'
--		| mark >= 50 = 'C'
--		| mark >= 40 = 'D'
--		| otherwise = 'F'

gradeAlt :: Int -> Char
gradeAlt mark
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
gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents list = [(i,(gradeAlt j)) | (i,j) <- list] 

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
splitMapVer input = (fst <$> input,snd <$> input) --TODO use list comprehension not map

split :: [(a,b)] -> ([a],[b])
--split input =  [ (\ (x,y) -> ([x],[y])) | (x,y) <- input]
--split input = [([fst i],[snd i]) | i <- input]


main = do 
	print $ splitMapVer [(1,'a'),(2,'b'),(3,'c')]
--	print $ capMarks [("Test0",50),("Test1",30)]
	print $ gradeStudents [("Jo",47), ("Sam",76)]
	
