import qualified Data.List




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

--Exercise 4
--Exercise 5
--Exercise 6
--Exercise 7
--Exercise 8
--Exercise 9
--Exercise 10
--Exercise 11
--sunsequence :: [Int] -> [Int] -> Bool
subsequenceAlt :: [Int] -> [Int] -> Bool
subsequenceAlt list list2 = list `Data.List.isSubsequenceOf` list2


main =do


	 putStrLn "EOP"
