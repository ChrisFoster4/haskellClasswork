--Enumerated types
--Exercise 1
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun deriving (Eq,Ord,Show,Read)
data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Eq,Ord,Show,Read)
data Season = Spring | Summer | Autumn | Winter deriving (Show,Read,Eq,Ord) --Compilation error without Eq
--Exercise 2

season :: Month -> Season
season month
	| month == December || month < March = Winter
	| month < June = Spring
	| month < September = Summer
	| otherwise = Autumn

--Exercise 3
numberOfDays :: Month -> Int -> Int
numberOfDays month year
	  | month == February &&  year `mod` 4 == 0 = 29
	  | month == February = 28
	  | (month == April ||month ==  June ||month == September || month == November) = 30 --Is this line needlessly long?
	  | otherwise = 31

--Points and Shapes
--Exercise 4
data Point = Point Float  Float deriving (Show,Read,Eq,Ord)
--Exercise 5
data Shape = Circle Float |
             Rectangle Float Float deriving (Show,Read,Eq,Ord)

data PositionedShape = PositionedShape Shape Point deriving (Show,Read,Eq,Ord)
--Exercise 6
move :: PositionedShape -> Float -> Float -> PositionedShape
move (PositionedShape shape (Point x y)) xChange yChange = (PositionedShape shape (Point (x + xChange) (y + yChange)))

--Functions for the binary tree type
-- Binary tree algebraic type
data Tree = Null |
     Node Int Tree Tree
     deriving (Show)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
                  (Node 8 (Node 4 (Node 6 Null Null) Null) Null)
--Exercise 7
numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node n lst rst) = 1 + numberOfNodes lst + numberOfNodes rst

--Exercise 8
isMember :: Int -> Tree -> Bool
isMember _ Null = False
isMember toSearchFor (Node n lst rst) = if n == toSearchFor then True else isMember toSearchFor lst || isMember toSearchFor rst

--Exercise 9
--If it has a subtree then it isn't a leaf node
leaves :: Tree -> [Int]
leaves (Node n Null Null) = [n]
leaves (Node n lst Null) = leaves lst
leaves (Node n Null rst ) = leaves rst
leaves (Node n lst rst) = leaves lst ++ leaves rst

--Exercise 10
--Left sub tree centre node right sub tree
inOrder :: Tree -> [Int]
inOrder (Node n Null Null) = [n]
inOrder (Node n lst Null) = inOrder lst ++ [n]
inOrder (Node n Null rst) = [n] ++ inOrder rst
inOrder (Node n lst rst)  = inOrder lst ++ [n] ++ inOrder rst

--Exercise 11
insert :: Int -> Tree -> Tree
insert toInsert (Node n rst lst) = if (n >= rst) then insert toInsert lst else insert toInsert rst
--Exercise 12



main = do
	--putStrLn "Testing PositionedShape"
	--let circle = Circle 5
	--let point = Point 5 5
	--let x = PositionedShape circle point
	--let y = move x 7 7
	--print y
	putStrLn "testing trees"
	-- print $ numberOfNodes testTree
	-- print $ isMember 20 testTree
	print $ leaves testTree
	print $ inOrder testTree
	putStrLn "EOP"
