--import qualified Data.List(isSubsequenceOf) --Used in ex13

--Exercise 1
--Return the val of head + 1
headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne (x:_) = (head [x] + 1) --TODO Could this only be two lines?
--headPlusOne [n] = (head [n] + 1) --Non exhaustive pattern if line removed.



--Exercise 2
--Add the value at the start of the list before the list
duplicateHead :: [Int] -> [Int]
duplicateHead [] = []
duplicateHead (x:xs) = x:(x:xs)


--Exercise 3
rotate :: [a] -> [a]
rotate (x:y:xs) = y:x:xs
rotate xs = xs

--Exercise 4
--Recreate length function
listLength :: [a] -> Int
listLength (x:xs) = 1 + listLength xs
listLength [] = 0

--Exercise 5
multAllAlt :: [Int] -> Int
multAllAlt list = product list

multAll :: [Int] -> Int
multAll (x:xs) = x * multAll xs
multAll [] = 1

--Exercise 6
andAllAlt :: [Bool] -> Bool
andAllAlt list = not$ elem False list

andAll :: [Bool] -> Bool
andAll (x:xs) = if x == False then False else andAll xs
andAll [] = True

--Exercise 7
--return the number of times the first argument appears in the list
countElemsAlt :: Int -> [Int] -> Int
countElemsAlt num list = length $ filter (==num) list

countElems :: Int -> [Int] -> Int
countElems  y (x:xs) = if x == y then 1 + (countElems y xs) else countElems y xs
countElems y [] = 0 --I expected this to make it always return 0.

--Exercise 8
removeAllAlt :: Int -> [Int] -> [Int]
removeAllAlt num list = filter (/=num) list

removeAll :: Int -> [Int] -> [Int]
removeAll num (x:xs) = if x == num then removeAll num xs else x :(removeAll num xs)
removeAll num [] = []

--Exercise 9
type StudentMark = (String,Int)
listMarks :: String -> [(String,Int)] -> [Int]
listMarks student (x:xs) = if fst x == student then snd x : (listMarks student xs) else listMarks student xs
listMarks student [] = []

--Exercise 10
--Check if the first argument is a prefix to the second argument
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix (y:ys) (x:xs) = if y /= x then False else prefix ys xs
prefix (x:xs) [] = False

--Exercise 11
subsequences :: [Int] -> [Int] -> Bool
subsequences ys (x:xs) = if prefix ys (x:xs) then True else subsequences ys (xs)
subsequences (y:ys) [] = False
subsequences [] [] = True



--subsequenceAlt :: [Int] -> [Int] -> Bool
--subsequenceAlt list list2 = list `Data.List.isSubsequenceOf` list2

subsequencesBroken :: [Int] -> [Int] -> Bool
subsequencesBroken (y:ys) (x:xs) = if y == x then subsequences ys xs else subsequences (y:ys) xs
subsequencesBroken [] (x:xs) = True
subsequencesBroken (_) (_) = False

ss :: [Int] -> [Int] -> Bool
ss _ [] = False
ss [] _ = True
ss x (y:ys) = False || prefix x ys




main = do
	--print $ listMarks "Joe" [("Joe", 45), ("Sam", 70), ("Joe", 52)]
	--print $ prefix [1,4] [1,4,9,2]
	--print $ prefix [1,4,3] [1,4,9,2]
	putStrLn "Should be True"
	print $ subsequences [1,4,9] [8,1,4,2,1,4,9] --Broken
	print $ subsequences [1,4,9] [8,1,4,9,2,1]
	print $ subsequences [8,1,4] [8,1,4,9,2,1]
	print $ subsequences [4,9,2] [8,1,4,9,2,1]
	putStrLn "Should be False"
	print $ subsequences [1,4,2,1,4,9,1] [8,1,4,9,2,1]
	print $ subsequences [1,4,9,1] [8,1,4,9,2,1]
	print $ subsequences [8,2,4] [8,1,4,9,2,1]
	print $ subsequences [8,5,4] [8,1,4,9,2,1]
	putStrLn "EOP"
