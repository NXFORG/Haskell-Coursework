--
-- MATHFUN
-- 854443
--
--

import Data.Char
import Data.List
import Data.List (sortBy)
import Data.Ord (comparing)

-- Types

--
-- Define Album type here
data Album = Album{title, artist :: String, year, sales :: Int } deriving (Show, Read)

-- testData containing a list of the 50 albumToString

testData :: [Album]
testData = [(Album "Greatest Hits" "Queen" 1981 6300000), (Album "Gold: Greatest Hits" "ABBA" 1992 5400000),
            (Album "Sgt. Pepper's Lonely Hearts Club Band"  "The Beatles"  1967  5340000),  (Album "21"  "Adele"  2011  5110000),  (Album "(What's the Story) Morning Glory?"  "Oasis"  1995  4940000),
            (Album "Thriller"  "Michael Jackson"  1983  4470000),  (Album "The Dark Side of the Moon"  "Pink Floyd"  1973  4470000),  (Album "Brothers in Arms"  "Dire Straits"  1985  4350000),
            (Album "Bad"  "Michael Jackson"  1987  4140000),  (Album "Rumours"  "Fleetwood Mac"  1977  4090000),  (Album "Greatest Hits II"  "Queen"  1991  3990000),
            (Album "Back to Black"  "Amy Winehouse"  2006  3940000),  (Album "The Immaculate Collection"  "Madonna"  1990  3700000),  (Album "25"  "Adele"  2015  3500000),
            (Album "Stars"  "Simply Red"  1991  3450000),  (Album "Come On Over"  "Shania Twain"  1998  3430000),  (Album "x"  "Ed Sheeran"  2014  3380000),
            (Album "Legend"  "Bob Marley"  1984  3380000),  (Album "Bat Out of Hell"  "Meat Loaf"  1977  3370000),  (Album "Back to Bedlam"  "James Blunt"  2004  3360000),
            (Album "Urban Hymns"  "The Verve"  1997  3340000),  (Album "Bridge over Troubled Water"  "Simon & Garfunkel"  1970  3260000),  (Album "1"  "The Beatles"  2000  3230000),
            (Album "Spirit"  "Leona Lewis"  2007  3170000),  (Album "Crazy Love"  "Michael Bublé"  2009  3130000),  (Album "No Angel"  "Dido"  2000  3090000),
            (Album "White Ladder"  "David Gray"  1998  3020000),  (Album "The Fame"  "Lady Gaga"  2009  2990000),  (Album "Only by the Night"  "Kings of Leon"  2008  2980000),
            (Album "A Rush of Blood to the Head"  "Coldplay"  2002  2960000),  (Album "Talk on Corners"  "The Corrs"  1997  2960000),  (Album "Spice"  "Spice Girls"  1996  2960000),
            (Album "Life for Rent"  "Dido"  2003  2900000),  (Album "Beautiful World"  "Take That"  2006  2880000),  (Album "The Joshua Tree"  "U2"  1987  2880000),
            (Album "Hopes and Fears"  "Keane"  2004  2860000),  (Album "The War of the Worlds"  "Jeff Wayne"  1978  2800000),  (Album "X&Y"  "Coldplay"  2005  2790000),
            (Album "Jagged Little Pill"  "Alanis Morissette"  1995  2780000),  (Album "Tubular Bells"  "Mike Oldfield"  1973  2760000),  (Album "Scissor Sisters"  "Scissor Sisters"  2004  2760000),
            (Album "...But Seriously"  "Phil Collins"  1989  2750000),  (Album "Tracy Chapman"  "Tracy Chapman"  1988  2710000),  (Album "Parachutes"  "Coldplay"  2000  2710000),
            (Album "The Man Who"  "Travis"  1999  2687500),  (Album "Greatest Hits"  "ABBA"  1975  2606000),  (Album "I've Been Expecting You"  "Robbie Williams"  1998  2586500),
            (Album "Come Away with Me"  "Norah Jones"  2002  2556650),  (Album "Graceland"  "Paul Simon"  1986  2500000),  (Album "Ladies & Gentlemen: The Best of"  "George Michael"  1998  2500000),(Album "Yes" "Queen" 1987 7600000)]

--
--
--  Your functional code goes here

-- Converts a list of albums to a string

albumsToString :: [Album] -> String
albumsToString  [] = " "
albumsToString  (x:xs) = stringAlbum x ++ "\n" ++ albumsToString xs

-- Formats each category into a column of specified width

stringAlbum :: Album -> String
stringAlbum (Album {title = t, artist = a, year = y, sales= s}) =
   columnify 41 t ++ columnify 21 a ++ columnify 6 (show y) ++ columnify 9 (show s)

-- Gets the length value of the longest piece of data in each field and fills the gap
-- between shorter values and the max length with spaces

columnify :: Int -> String -> String
columnify maxLength value = value ++ (take (maxLength - (length value)) (repeat ' '))

-- Gets the top 10 highest selling albums, sorted in descending order

top10 :: [Album] -> [Album]
top10 albumList = take 10 $ sortBy (flip $ comparing sales) albumList

-- Gets all albums released between and including two user-specified dates

getBetween :: Int -> Int -> [Album] -> [Album]
getBetween a b [] = []
getBetween a b (x:xs)
      | year x < a || year x > b = getBetween a b xs
      | year x >= a && year x <= b = [x] ++ getBetween a b xs

-- Gets all albums starting with a given prefix

getPrefix :: String -> [Album] -> [Album]
getPrefix a [] = []
getPrefix pref (x:xs)
      | isPrefixOf pref (title x) == False = getPrefix pref xs
      | isPrefixOf pref (title x) == True = [x] ++ getPrefix pref xs

-- Gets the total amount of sales from a given artist

getSales :: String -> [Album] -> Int
getSales artist albums = sum [ sales | Album _ a _ sales <- albums, a == artist ]

-- Gets a list of albums and displays a list of artists and the number of their albums in the top 50

albumNo :: [Album] -> String
albumNo [] = " "
albumNo (Album _ aut _ _:xs) = columnify 20 aut ++ columnify 2 (albumTotal aut testData) ++
  " \n" ++ (albumNo (filter(\(Album _ a _ _) -> aut /= a) xs))

-- Gets the total number of albums in the top 50 for each artist

albumTotal :: String -> [Album] -> String
albumTotal artist db = show $ sum  [ 1 | Album _ a _ _ <- db, a == artist ]

-- Adds and album to a list of albums

addAlbum :: String -> String -> Int -> Int -> [Album] -> [Album]
addAlbum title artist year sales db = (Album title artist year sales) : db

-- Removes the last album from the List

removeLast :: [Album] -> [Album]
removeLast xs = tail (init xs)

removeElementAlbum:: [Album] -> ([Album] -> [Album]) -> [Album]
removeElementAlbum x func = func x

--

addSales :: String -> String  -> [Album] -> [Album]
addSales ttl art [] = []
addSales ttl art (x:xs)
    | title x == ttl = drop (getIndex ttl (x:xs)) xs
    | otherwise = addSales ttl art xs

-- Gets the index in the list of an album

getIndex:: String -> [Album]-> Int
getIndex _  [] = (-1)
getIndex ttl (Album title _ _ _:xs)
    | ttl == title = 0
    | otherwise = 1+ getIndex ttl xs

-- Demo function to test basic functionality (without persistence - i.e.
-- testData doesn't change and nothing is saved/loaded to/from albums file).

demo :: Int -> IO ()
demo 1  = putStrLn (albumsToString testData)
demo 2  = putStrLn (albumsToString (top10 testData))
demo 3  = putStrLn (albumsToString (getBetween 2000 2008 testData))
demo 4  = putStrLn (albumsToString (getPrefix "Th" testData))
demo 5  = putStrLn (show (getSales "Queen" testData))
demo 6  = putStrLn (albumNo testData)
demo 7  = putStrLn (albumsToString (addAlbum "Progress" "Take That" 2010 2700000 $ removeLast testData))
demo 8  = putStrLn (albumsToString (addSales "21" "Adele" testData))

--
--
-- Your user interface (and loading/saving) code goes here
--

main :: IO ()
main = do
  db <- getFile
  putStrLn (albumsToString db)

getFile :: IO [Album]
getFile = do
    file <- readFile "albums.txt"
    length file `seq` return (read file :: [Album])





exitFile :: [Album] -> IO ()
exitFile albums = do writeFile "albums.txt" (show albums)



--
