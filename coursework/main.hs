--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
--

--
-- Types
--
-- Define Film type here

-- testDatabase :: [Film]
-- testDatabase = [ ... the 25 Film values ... ]

--
--
--  Your functional code goes here
--
--

-- Demo function to test basic functionality (without persistence - i.e.
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

--demo :: Int -> IO ()
--demo 1  = putStrLn all films after adding 2018 film "Sherlock Gnomes"
--          directed by by "John Stevenson" to testDatabase
--demo 2  = putStrLn (filmsAsString testDatabase)
--demo 3  = putStrLn all films by "Ridley Scott"
--demo 4  = putStrLn all films with website rating >= 75%
--demo 5  = putStrLn average website rating for "Ridley Scott"
--demo 6  = putStrLn titles of films rated by "Emma" (with likes/dislikes)
--demo 7  = putStrLn all films after "Emma" says she likes "Avatar"
--demo 71 = putStrLn all films after "Emma" says she likes "Titanic"
--demo 72 = putStrLn all films after "Emma" says she dislikes "Jaws"
--demo 8  = films between 2000 and 2006 inclusive sorted by website rating

--
--
-- Your user interface code goes here
--
--
type Title = String
type Director = String
type YearOfRelease = Int
type UsersWhoLike = [String]
type UsersWhoDislike = [String]

data Film = Film Title Director YearOfRelease UsersWhoLike UsersWhoDislike deriving (Show,Read,Ord,Eq) --TODO check if it needs to derive all these type classes
main :: IO()
main = do
    let y = [Film]
    let x = Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"] ["Sam", "Olga", "Tim"]
    print x
    print $ y ++ x
    

    putStrLn "EOP"
