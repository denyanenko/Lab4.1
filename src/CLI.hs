module CLI where

import Options.Applicative
import Data.Semigroup ((<>))
import MyLib
import System.IO (hFlush, stdout)

data Command
    = AddBook [String] String String String Int Int
    | AddArticle [String] String String Int Int (Int, Int)
    | AddThesis [String] String String String Int (Int, Int)
    | ListPublications
    | FindByAuthor String
    | FindSoloByAuthor String
    | GetStatistics
    | GetPublishersJournalsConferences
    deriving (Show)

commandParser :: Parser Command
commandParser = subparser
    ( command "add-book"
        (info (AddBook
               <$> some (strOption (long "author" <> short 'a' <> metavar "AUTHOR" <> help "Author of the book"))
               <*> strOption (long "title" <> short 't' <> metavar "TITLE" <> help "Title of the book")
               <*> strOption (long "city" <> short 'c' <> metavar "CITY" <> help "City of the publisher")
               <*> strOption (long "publisher" <> short 'p' <> metavar "PUBLISHER" <> help "Publisher of the book")
               <*> option auto (long "year" <> short 'y' <> metavar "YEAR" <> help "Year of publication")
               <*> option auto (long "pages" <> short 'n' <> metavar "PAGES" <> help "Number of pages")
             ) (progDesc "Add a new book"))
    <> command "add-article"
        (info (AddArticle
               <$> some (strOption (long "author" <> short 'a' <> metavar "AUTHOR" <> help "Author of the article"))
               <*> strOption (long "title" <> short 't' <> metavar "TITLE" <> help "Title of the article")
               <*> strOption (long "journal" <> short 'j' <> metavar "JOURNAL" <> help "Journal of the article")
               <*> option auto (long "year" <> short 'y' <> metavar "YEAR" <> help "Year of publication")
               <*> option auto (long "journal-number" <> short 'i' <> metavar "NUMBER" <> help "Journal number")
               <*> option auto (long "pages" <> short 'p' <> metavar "PAGES" <> help "Pages (start,end)")
             ) (progDesc "Add a new article"))
    <> command "add-thesis"
        (info (AddThesis
               <$> some (strOption (long "author" <> short 'a' <> metavar "AUTHOR" <> help "Author of the thesis"))
               <*> strOption (long "title" <> short 't' <> metavar "TITLE" <> help "Title of the thesis")
               <*> strOption (long "conference" <> short 'c' <> metavar "CONFERENCE" <> help "Conference of the thesis")
               <*> strOption (long "city" <> short 'i' <> metavar "CITY" <> help "City of the conference")
               <*> option auto (long "year" <> short 'y' <> metavar "YEAR" <> help "Year of publication")
               <*> option auto (long "pages" <> short 'p' <> metavar "PAGES" <> help "Pages (start,end)")
             ) (progDesc "Add a new thesis"))
    <> command "list"
        (info (pure ListPublications) (progDesc "List all publications"))
    <> command "find-by-author"
        (info (FindByAuthor
               <$> strOption (long "author" <> short 'a' <> metavar "AUTHOR" <> help "Author to find")
             ) (progDesc "Find publications by author"))
    <> command "find-solo-by-author"
        (info (FindSoloByAuthor
               <$> strOption (long "author" <> short 'a' <> metavar "AUTHOR" <> help "Author to find solo publications")
             ) (progDesc "Find solo publications by author"))
    <> command "get-statistics"
        (info (pure GetStatistics) (progDesc "Get publication type statistics"))
    <> command "get-publishers-journals-conferences"
        (info (pure GetPublishersJournalsConferences) (progDesc "Get unique publishers, journals, and conferences"))
    )

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
    ( fullDesc
    <> progDesc "Manage your publications"
    <> header "Publications Manager - a simple tool to manage your publications" )

runCLI :: IO ()
runCLI = do
    command <- execParser opts
    handleCommand command

handleCommand :: Command -> IO ()
handleCommand (AddBook authors title city publisher year pages) = do
    putStrLn "AddBook command not implemented yet"
handleCommand (AddArticle authors title journal year journalNumber pages) = do
    -- тут додайте логіку для додавання статті
    putStrLn "AddArticle command not implemented yet"
handleCommand (AddThesis authors title conference city year pages) = do
    -- тут додайте логіку для додавання тези
    putStrLn "AddThesis command not implemented yet"
handleCommand ListPublications = do
    -- тут додайте логіку для відображення всіх публікацій
    putStrLn "ListPublications command not implemented yet"
handleCommand (FindByAuthor author) = do
    -- тут додайте логіку для пошуку публікацій за автором
    putStrLn "FindByAuthor command not implemented yet"
handleCommand (FindSoloByAuthor author) = do
    -- тут додайте логіку для пошуку одноосібних публікацій за автором
    putStrLn "FindSoloByAuthor command not implemented yet"
handleCommand GetStatistics = do
    -- тут додайте логіку для отримання статистики
    putStrLn "GetStatistics command not implemented yet"
handleCommand GetPublishersJournalsConferences = do
    -- тут додайте логіку для отримання унікальних видавництв, журналів та конференцій
    putStrLn "GetPublishersJournalsConferences command not implemented yet"
