module MyLib where

import Data.Monoid
import Data.Semigroup
import Data.List (find, nub, intercalate)
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative
-- Оголошення типу даних для автора
type Author = String

-- Оголошення типу даних для публікації
data Publication =
    Book {
        authors :: [Author],
        title :: String,
        city :: String,
        publisher :: String,
        year :: Int,
        pageCount :: Int
    }
    | Article {
        authors :: [Author],
        title :: String,
        journal :: String,
        year :: Int,
        journalNumber :: Int,
        pages :: (Int, Int) -- (start page, end page)
    }
    | Thesis {
        authors :: [Author],
        title :: String,
        conference :: String,
        city :: String,
        year :: Int,
        pages :: (Int, Int) -- (start page, end page)
    }
    deriving (Show, Eq)

-- Власний клас типів для публікацій
class PublicationType publication where
    getType :: publication -> String
    getSource :: publication -> String
    
-- Втілення класу типів для книги
instance PublicationType Publication where
    getType (Book {}) = "Book"
    getType (Article {}) = "Article"
    getType (Thesis {}) = "Thesis"

    getSource (Book { publisher = pub }) = pub
    getSource (Article { journal = j }) = j
    getSource (Thesis { conference = c }) = c

-- Оголошення типу даних для списку публікацій
newtype Publications = Publications [Publication] deriving (Show)

-- Втілення класу Monoid для списку публікацій
instance Semigroup Publications where
    (Publications xs) <> (Publications ys) = Publications (xs <> ys)

instance Monoid Publications where
    mempty = Publications []

-- Функція для визначення типу публікації за назвою
publicationType :: String -> Publications -> Maybe String
publicationType name (Publications publications) =
    case find (\p -> title p == name) publications of
        Just pub -> Just (getType pub)
        Nothing -> Nothing

-- Функція для пошуку усіх публікацій вказаного автора
findPublicationsByAuthor :: Publications -> String -> Publications
findPublicationsByAuthor (Publications pubs) auth = Publications $ filter (\p -> auth `elem` authors p) pubs

-- Функція для пошуку усіх одноосібних публікацій вказаного автора
findSoloPublicationsByAuthor :: Publications -> String -> Publications
findSoloPublicationsByAuthor (Publications pubs) auth = Publications $ filter (\p -> length (authors p) == 1 && auth `elem` authors p) pubs

-- Функція для статистики по базі за типом публікацій
publicationTypeStatistics :: Publications -> [(String, Int)]
publicationTypeStatistics (Publications pubs) = 
    let getTypeCount typ = length $ filter (\p -> getType p == typ) pubs
        bookCount = getTypeCount "Book"
        articleCount = getTypeCount "Article"
        thesisCount = getTypeCount "Thesis"
    in [("Book", bookCount), ("Article", articleCount), ("Thesis", thesisCount)]

-- Функція для отримання унікальних значень видавництв, журналів та конференцій
getAllPublishersJournalsConferences :: Publications -> [(String, [String])]
getAllPublishersJournalsConferences (Publications publications) = [
        ("Publishers:", nub publishers),
        ("Journals:", nub journals),
        ("Conferences:", nub conferences)
    ]
    where
        publishers = map getSource $ filter (\p -> getType p == "Book") publications
        journals = map getSource $ filter (\p -> getType p == "Article") publications
        conferences = map getSource $ filter (\p -> getType p == "Thesis") publications

-- Функція для виведення публікації з форматованим виглядом
printPublication :: Publication -> IO ()
printPublication (Book authors title city publisher year pageCount) =
    putStrLn $ "Book: " ++ title ++ " (" ++ show year ++ "), by " ++ formatAuthors authors ++ ", published by " ++ publisher ++ ", " ++ city ++ ", " ++ show pageCount ++ " pages"
printPublication (Article authors title journal year journalNumber (startPage, endPage)) =
    putStrLn $ "Article: " ++ title ++ " (" ++ show year ++ "), by " ++ formatAuthors authors ++ ", published in " ++ journal ++ ", №" ++ show journalNumber ++ ", pages " ++ show startPage ++ "-" ++ show endPage
printPublication (Thesis authors title conference city year (startPage, endPage)) =
    putStrLn $ "Thesis: " ++ title ++ " (" ++ show year ++ "), by " ++ formatAuthors authors ++ ", presented at " ++ conference ++ ", " ++ city ++ ", pages " ++ show startPage ++ "-" ++ show endPage

-- Функція для форматування списку авторів
formatAuthors :: [Author] -> String
formatAuthors  = intercalate ", " 

printPublications :: Publications -> IO ()
printPublications (Publications pubs) = mapM_ printPublication pubs

instance Read Publication where
    readsPrec _ = readP_to_S publicationParser

publicationParser :: ReadP Publication
publicationParser = choice [bookParser, articleParser, thesisParser]

bookParser :: ReadP Publication
bookParser = do
    skipSpaces
    _ <- string "Book {"
    authors <- field "authors" listParser
    title <- field "title" stringParser
    city <- field "city" stringParser
    publisher <- field "publisher" stringParser
    year <- field "year" intParser
    pageCount <- field "pageCount" intParser
    _ <- char '}'
    return $ Book authors title city publisher year pageCount

articleParser :: ReadP Publication
articleParser = do
    skipSpaces
    _ <- string "Article {"
    authors <- field "authors" listParser
    title <- field "title" stringParser
    journal <- field "journal" stringParser
    year <- field "year" intParser
    journalNumber <- field "journalNumber" intParser
    pages <- field "pages" tupleParser
    _ <- char '}'
    return $ Article authors title journal year journalNumber pages

thesisParser :: ReadP Publication
thesisParser = do
    skipSpaces
    _ <- string "Thesis {"
    authors <- field "authors" listParser
    title <- field "title" stringParser
    conference <- field "conference" stringParser
    city <- field "city" stringParser
    year <- field "year" intParser
    pages <- field "pages" tupleParser
    _ <- char '}'
    return $ Thesis authors title conference city year pages

field :: String -> ReadP a -> ReadP a
field name parser = do
    skipSpaces
    _ <- string name
    skipSpaces
    _ <- char '='
    skipSpaces
    value <- parser
    skipSpaces
    Control.Applicative.optional (char ',')
    return value

listParser :: ReadP [String]
listParser = between (char '[') (char ']') (sepBy stringParser (skipSpaces >> char ',' >> skipSpaces))

stringParser :: ReadP String
stringParser = between (char '"') (char '"') (munch1 (/= '"'))

intParser :: ReadP Int
intParser = read <$> munch1 (`elem` "0123456789")

tupleParser :: ReadP (Int, Int)
tupleParser = do
    _ <- char '('
    skipSpaces
    a <- intParser
    skipSpaces
    _ <- char ','
    skipSpaces
    b <- intParser
    skipSpaces
    _ <- char ')'
    return (a, b)

someFunc :: IO ()
someFunc = do
    let publications = Publications [
            Book ["Author1"] "Title1" "City1" "Publisher1" 2022 300,
            Article ["Author2"] "Title2" "Journal1" 2023 1 (10, 20),
            Thesis ["Author3"] "Title3" "Conference1" "City2" 2021 (30, 40),
            Article ["Author1", "Author4"] "Title4" "Journal2" 2024 2 (15, 25),
            Book ["Author5"] "Title5" "City3" "Publisher2" 2023 250,
            Thesis ["Author6"] "Title6" "Conference2" "City4" 2022 (50, 60)
            ]
        publicationName = "Title1"
        authorName = "Author1"

    putStrLn "All publications:"
    printPublications publications
    
    putStrLn $ "\nPublication type for " ++ publicationName ++ ":"
    case publicationType publicationName publications of
        Just pubType -> putStrLn pubType
        Nothing -> putStrLn "Publication not found"
    
    putStrLn $ "\nPublications by " ++ authorName ++ ":"
    printPublications $ findPublicationsByAuthor publications authorName
    
    putStrLn $ "\nSolo publications by " ++ authorName ++ ":"
    printPublications $ findSoloPublicationsByAuthor publications authorName

    putStrLn "\nPublication type statistics:"
    print $ publicationTypeStatistics publications

    putStrLn "\nUnique publishers, journals, and conferences:"
    let categories = getAllPublishersJournalsConferences publications
    mapM_ (\(category, items) -> do
        putStrLn category
        mapM_ putStrLn items
        putStrLn ""
        ) categories





   