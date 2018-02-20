-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
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

-- Your user interface code goes

data Film = Film
 { title :: String
 , director :: String
 , yearOfRelease :: Int
 , usersWhoLike :: [String]
 , usersWhoDislike :: [String]
 } deriving (Show,Read,Ord,Eq) --TODO check if all these 4 classes need to be derivived

addFilm :: Film -> [Film] -> [Film]
addFilm newFilm listOfFilms = [newFilm] ++ listOfFilms

showFilms :: [Film] -> IO ()
showFilms listOfFilms = print listOfFilms

filterFilmByDirector :: String -> [Film] -> [Film]
filterFilmByDirector directorName listOfFilms = filter ((==directorName) . director) listOfFilms

outputFilmTitles :: [Film] -> [String]
outputFilmTitles ((Film title _ _ _ _):xs) = [title] ++ outputFilmTitles xs
outputFilmTitles [] = []

testFilm = Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"] ["Sam", "Olga", "Tim"]
testFilmAlt = Film "Not Blade Runner" "Not Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"] ["Sam", "Olga", "Tim"]
testFilms = [testFilm] ++ [testFilm] ++ [testFilm] ++ [testFilmAlt]

main :: IO()
main = do
    let y = [Film]
    let x = Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"] ["Sam", "Olga", "Tim"]
    print x
    -- print $ y ++ x
    putStrLn "EOP"
