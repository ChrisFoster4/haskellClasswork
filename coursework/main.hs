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
outputFilmTitles ((Film title _ _ _ _):xs) = [title] ++ outputFilmTitles xs --TODO Do this non recurrsively with a similar method to filterFilmByDirector?
outputFilmTitles [] = []

testFilmAlt = Film "Not Blade Runner" "Not Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"] ["Sam", "Olga", "Tim"]
testFilm = Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"] ["Sam", "Olga", "Tim"]
testFilms = [testFilm] ++ [testFilm] ++ [testFilm] ++ [testFilmAlt]

testDatabase = [
                 Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"] ["Sam", "Olga", "Tim"]
               , Film "The Fly" "David Cronenberg" 1986 ["Garry", "Dave", "Zoe"] ["Kevin", "Emma", "Heidi", "Jo", "Kate"]
               , Film "Body Of Lies" "Ridley Scott" 2008 ["Garry", "Dave"] ["Bill", "Olga", "Tim", "Zoe", "Paula"]
               , Film "Avatar" "James Cameron" 2009 ["Dave", "Amy", "Liz"] ["Olga", "Tim", "Zoe", "Paula"]
               , Film "Titanic" "James Cameron" 1997 ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"] ["Sam", "Wally", "Kate"]
               , Film "The Departed" "Martin Scorsese" 2006 ["Wally", "Liz", "Kevin", "Tim", "Emma"] ["Olga", "Dave", "Kate", "Zoe"]
               , Film "Aliens" "Ridley Scott" 1986 ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"] ["Tim", "Emma", "Jo", "Olga"]
               , Film "Kingdom Of Heaven" "Ridley Scott" 2005 ["Jo", "Wally", "Emma"] ["Tim", "Garry", "Ian", "Neal"]
               , Film "Alien: Covenant" "Ridley Scott" 2017 ["Kevin", "Tim"] ["Emma", "Jo", "Liz"]
               , Film "E.T. The Extra-Terrestrial" "Steven Spielberg" 1982 ["Dave", "Amy", "Garry", "Ian", "Neal"] ["Jenny", "Kate", "Emma", "Olga"]
               , Film "Bridge of Spies" "Steven Spielberg" 2015 ["Wally", "Sam", "Dave", "Neal"] ["Bill", "Garry", "Ian", "Kate"]
               , Film "Jaws" "Steven Spielberg" 1975 ["Jenny", "Emma", "Bill", "Neal"] ["Sam", "Ian", "Kate"]
               , Film "The Martian" "Ridley Scott" 2015 ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"] ["Ian", "Neal", "Tim", "Liz"]
               , Film "The BFG" "Steven Spielberg" 2016 ["Sam", "Wally", "Dave", "Jo", "Kate"] ["Neal"]
               , Film "The Shawshank Redemption" "Frank Darabont" 1994 ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe", "Heidi"] ["Jo"]
               , Film "Gladiator" "Ridley Scott" 2000 ["Olga", "Neal", "Kate", "Garry"] ["Heidi", "Bill", "Sam", "Zoe"]
               , Film "The Green Mile" "Frank Darabont" 1999 ["Kevin", "Tim", "Emma", "Heidi"] ["Kate", "Jenny", "Zoe"]
               , Film "True Lies" "James Cameron" 1994 ["Sam", "Dave"] ["Emma", "Olga", "Jenny", "Zoe"]
               , Film "Super 8" "J J Abrams" 2011 ["Kevin", "Tim", "Emma", "Olga", "Heidi"] ["Emma", "Dave", "Jenny", "Zoe"]
               , Film "Minority Report" "Steven Spielberg" 2002 ["Kevin", "Kate", "Tim", "Emma", "Jenny", "Zoe"] ["Olga", "Heidi"]
               , Film "War Horse" "Steven Spielberg" 2011 ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"] ["Heidi", "Jenny", "Sam"]
               , Film "Silence" "Martin Scorsese" 2016 ["Wally", "Emma", "Tim", "Heidi", "Bill", "Jo"] ["Dave", "Olga"]
               , Film "The Terminal" "Steven Spielberg" 2004 ["Kate", "Dave", "Jo", "Wally", "Emma"] ["Heidi"]
               , Film "Star Wars: The Force Awakens" "J J Abrams" 2015 ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz"] ["Olga", "Jo", "Wally"]
               , Film "Hugo" "Martin Scorsese" 2011 ["Wally", "Sam"] ["Kate", "Bill", "Dave"]
               ]

main :: IO()
main = do
    let y = [Film]
    let x = Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"] ["Sam", "Olga", "Tim"]

    print $ filterFilmByDirector "Ridley Scott" testDatabase
    print $ outputFilmTitles $ filterFilmByDirector "Ridley Scott" testDatabase
    putStrLn "EOP"
