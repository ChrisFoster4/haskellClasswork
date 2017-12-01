import qualified Data.List(isSubsequenceOf) --Used in ex13

--Exercise 1
--Return the val of head + 1
headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne [n,_] = (head [n] + 1) --TODO make this only two lines
headPlusOne [n] = (head [n] + 1)


--Exercise 2
--Add the value at the start of the list before the list
duplicateHead :: [Int] -> [Int]
duplicateHead list = head list:list --TODO needs to return [] for [] as input


--Exercise 3
rotate :: [a] -> [a]
rotate (x:y:xs) = y:x:xs 
rotate n = n

--Exercise 4
--Recreate length function
listLength :: [a] -> Int
listLength (x:xs) = 1 + listLength (xs)
listLength n = 0

--Exercise 5
multAll :: [Int] -> Int
multAll (x:xs) = x * multAll xs
multAll [] = 1

multAllAlt :: [Int] -> Int
multAllAlt list = product list

--Exercise 6
andAllAlt :: [Bool] -> Bool
andAllAlt list = if elem False list then False else True --Is there a better way to reverse this

andAll :: [Bool] -> Bool
andAll (x:xs) = if x == False then False else andAll xs
andAll [] = True

--Exercise 7
--return the number of times the first argument appears in the list
--countElems :: Int -> [Int] -> Int
--countElems num list = 
countElems :: Int -> [Int] -> Int
countElems  y (x:xs) = if x == y then 1 + (countElems y xs) else countElems y xs
countElems y [] = 0 --I expected this to make it always return 0.

countElemsAlt :: Int -> [Int] -> Int
countElemsAlt num list = length $ filter (==num) list

--Exercise 8
removeAllAlt :: Int -> [Int] -> [Int]
removeAllAlt num list = filter (/=num) list

removeAll :: Int -> [Int] -> [Int]
removeAll num (x:xs) = if x == num then removeAll num xs else x :(removeAll num xs)
removeAll num [] = []

--Exercise 9
type StudentMark = (String,Int)
--listMarks student [(x,xs)] = if fst x == student then snd x : (listMarks student xs)else listMarks student xs
listMarks :: String -> [(String,Int)] -> [Int]
listMarks student (x:xs) = if fst x == student then snd x : (listMarks student xs) else listMarks student xs
listMarks student [] = []

--Exercise 10

--Exercise 11
--sunsequence :: [Int] -> [Int] -> Bool
subsequenceAlt :: [Int] -> [Int] -> Bool
subsequenceAlt list list2 = list `Data.List.isSubsequenceOf` list2


main =do
	print $ listMarks "Joe" [("Joe", 45), ("Sam", 70), ("Joe", 52)] 
	putStrLn "EOP"
