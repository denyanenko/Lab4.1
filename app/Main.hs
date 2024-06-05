{-# LANGUAGE DeriveGeneric #-}
module Main where

import Options.Applicative
import GHC.Generics (Generic)
import qualified MyLib
import Text.Read (readMaybe)
import System.IO
import Data.Maybe (mapMaybe)
import Control.Exception (evaluate)
import Control.Monad
import MyLib (Publication(..), Author)
import Data.List.Split (splitOn)

-- Записує список публікацій у текстовий файл
writePublicationsToFile :: MyLib.Publications -> IO ()
writePublicationsToFile (MyLib.Publications pubs) = withFile "publications.txt" WriteMode $ \handle ->
    mapM_ (hPrint handle) pubs


readPublicationsFromFile :: IO MyLib.Publications
readPublicationsFromFile = do
    contents <- readFile "publications.txt"
    evaluate (length contents) -- Force evaluation to avoid lazy IO issues
    let pubStrings = lines contents
    let publications = mapMaybe readMaybe pubStrings
    return (MyLib.Publications publications)


outputResult :: Options -> String -> IO ()
outputResult opts result = do
    unless (silent opts) $ putStrLn result
    when (textOutput opts) $ appendFile "output.txt" (result ++ "\n")
    when (htmlOutput opts) $ appendFile "output.html" ("<div>" ++ (replaceNewlineWithHTMLBreak result) ++ "</div>")

replaceNewlineWithHTMLBreak :: String -> String
replaceNewlineWithHTMLBreak str = foldr (\c acc -> if c == '\n' then "<br>\n" ++ acc else c:acc) "" str

readPublications :: Options -> IO ()
readPublications opts = do
    publications <- readPublicationsFromFile
    outputResult opts $ "\n" ++ MyLib.formatPublications publications

addPublication :: Options -> MyLib.Publication -> IO ()
addPublication opts pub = do
    publications <- readPublicationsFromFile
    let updatedPublications = publications <> MyLib.Publications [pub]
    writePublicationsToFile updatedPublications
    outputResult opts "\nPublication added successfully"

-- Додавання книги
addBook :: Options -> [Author] -> String -> String -> String -> Int -> Int -> IO ()
addBook opts authors title city publisher year pageCount = do
    let book = Book { authors = authors, title = title, city = city, publisher = publisher, year = year, pageCount = pageCount }
    addPublication opts book

-- Додавання статті
addArticle :: Options -> [Author] -> String -> String -> Int -> Int -> (Int, Int) -> IO ()
addArticle opts  authors title journal year journalNumber pages = do
    let article = Article { authors = authors, title = title, journal = journal, year = year, journalNumber = journalNumber, pages = pages }
    addPublication opts article

-- Додавання тези
addThesis :: Options -> [Author] -> String -> String -> String -> Int -> (Int, Int) -> IO ()
addThesis opts  authors title conference city year pages = do
    let thesis = Thesis { authors = authors, title = title, conference = conference, city = city, year = year, pages = pages }
    addPublication opts thesis

deletePublication :: Options -> Int -> IO ()
deletePublication opts idx = do
    MyLib.Publications publications <- readPublicationsFromFile
    if idx < 0 || idx >= length publications
        then outputResult opts "\nInvalid index"
        else do
            let updatedPublications = take idx publications ++ drop (idx + 1) publications
            writePublicationsToFile (MyLib.Publications updatedPublications)
            outputResult opts "\nPublication deleted successfully"

updatePublication :: Options -> Int -> MyLib.Publication -> IO ()
updatePublication opts idx pub = do
    MyLib.Publications publications <- readPublicationsFromFile
    if idx < 0 || idx >= length publications
        then outputResult opts "\nInvalid index"
        else do
            let updatedPublications = take idx publications ++ [pub] ++ drop (idx + 1) publications
            writePublicationsToFile (MyLib.Publications updatedPublications)
            outputResult opts "\nPublication updated successfully"

updateCommand :: Options -> Int -> IO ()
updateCommand opts id = do
  putStrLn "Choose the type of publication to update (b(Book), a(Article), t(Thesis)):"
  hFlush stdout
  pubType <- getLine
  case pubType of
    "b" -> do
      putStrLn "Enter authors:"
      hFlush stdout
      authors <- getLine
      putStrLn "Enter title:"
      hFlush stdout
      title <- getLine
      putStrLn "Enter city:"
      hFlush stdout
      city <- getLine
      putStrLn "Enter publisher:"
      hFlush stdout
      publisher <- getLine
      putStrLn "Enter year:"
      hFlush stdout
      year <- readLn
      putStrLn "Enter page count:"
      hFlush stdout
      pageCount <- readLn
      let book = Book { authors = parseAuthors authors, title = title, city = city, publisher = publisher, year = year, pageCount = pageCount }
      updatePublication opts id book
    "a" -> do
      putStrLn "Enter authors:"
      hFlush stdout
      authors <- getLine
      putStrLn "Enter title:"
      hFlush stdout
      title <- getLine
      putStrLn "Enter journal:"
      hFlush stdout
      journal <- getLine
      putStrLn "Enter year:"
      hFlush stdout
      year <- readLn
      putStrLn "Enter journal number:"
      hFlush stdout
      journalNumber <- readLn
      putStrLn "Enter start page:"
      hFlush stdout
      startPage <- readLn
      putStrLn "Enter end page:"
      hFlush stdout
      endPage <- readLn
      let article = Article { authors = parseAuthors authors, title = title, journal = journal, year = year, journalNumber = journalNumber, pages =(startPage, endPage) }
      updatePublication opts id article
    "t" -> do
      putStrLn "Enter authors:"
      hFlush stdout
      authors <- getLine
      putStrLn "Enter title:"
      hFlush stdout
      title <- getLine
      putStrLn "Enter conference:"
      hFlush stdout
      conference <- getLine
      putStrLn "Enter city:"
      hFlush stdout
      city <- getLine
      putStrLn "Enter year:"
      hFlush stdout
      year <- readLn
      putStrLn "Enter start page:"
      hFlush stdout
      startPage <- readLn
      putStrLn "Enter end page:"
      hFlush stdout
      endPage <- readLn
      let thesis = Thesis { authors = parseAuthors authors, title = title, conference = conference, city = city, year = year, pages =(startPage, endPage) }
      updatePublication opts id thesis
    _ -> putStrLn "Invalid publication type"

pubType ::  Options -> String -> IO ()
pubType opts name = do
   publications <- readPublicationsFromFile
   outputResult opts $ "\nPublication type for " ++ name ++ ":"
   case MyLib.publicationType name publications  of
        Just pubType -> outputResult opts pubType
        Nothing -> outputResult opts "\nPublication not found"

pubByAuthor :: Options -> String -> IO()
pubByAuthor opts author = do
    publications <- readPublicationsFromFile
    outputResult opts $ "\nPublications by " ++ author ++ ":"
    outputResult opts $ MyLib.formatPublications $ MyLib.findPublicationsByAuthor publications author

soloPubByAuthor :: Options -> String -> IO()
soloPubByAuthor opts author = do
    publications <- readPublicationsFromFile
    outputResult opts $ "\nSolo publications by " ++ author ++ ":"
    outputResult opts $ MyLib.formatPublications $ MyLib.findSoloPublicationsByAuthor publications author

pubTypeStatistics :: Options -> IO()
pubTypeStatistics opts = do
    publications <- readPublicationsFromFile
    outputResult opts "\nPublication type statistics:"
    outputResult opts $ show $ MyLib.publicationTypeStatistics publications

getAllPubJourConf :: Options -> IO()
getAllPubJourConf opts = do
    publications <- readPublicationsFromFile
    outputResult opts "\nUnique publishers, journals, and conferences:"
    let categories = MyLib.getAllPublishersJournalsConferences publications
    mapM_ (\(category, items) -> do
        outputResult opts category
        mapM_ (outputResult opts) items
        outputResult opts ""
        ) categories

    
data Options = Options
  { readPub :: Bool
  , delete :: Maybe Int
  , pubTypeName :: Maybe String 
  , pubByAuthorName :: Maybe String 
  , soloPubByAuthorName :: Maybe String  
  , pubTypeStats :: Bool
  , allPubJourConf :: Bool
  , addB :: Maybe (String, String, String, String, Int, Int)
  , addA :: Maybe (String, String, String, Int, Int, Int, Int)
  , addT :: Maybe (String, String, String, String, Int, Int, Int)
  , update :: Maybe Int
  , silent :: Bool
  , textOutput :: Bool
  , htmlOutput :: Bool
  } deriving (Show, Generic)

  

options :: Parser Options
options = Options
      <$> switch ( long "readPub" <> short 'r'
                <> help "Read all publications" )
      <*> optional (option auto ( long "delete" <> short 'd'
                                <> metavar "ID"
                                <> help "Delete entry with specified ID"))
      <*> optional (strOption ( long "pubType" <> short 'p'
                              <> metavar "NAME"
                              <> help "Show publication type for specified name"))
      <*> optional (strOption ( long "pubByAuthor" <> short 'a'
                              <> metavar "AUTHOR"
                              <> help "Show publications by specified author"))
      <*> optional (strOption ( long "soloPubByAuthor" <> short 's'
                              <> metavar "AUTHOR"
                              <> help "Show solo publications by specified author"))
      <*> switch ( long "pubTypeStats"
                <> help "Show publication type statistics" )
      <*> switch ( long "allPubJourConf"
                <> help "Show unique publishers, journals, and conferences" )
      <*> optional (subparser
           (command "addBook"
             (info ( (,,,,,) <$> argument str (metavar "AUTHORS")
                         <*> argument str (metavar "TITLE")
                         <*> argument str (metavar "CITY")
                         <*> argument str (metavar "PUBLISHER")
                         <*> argument auto (metavar "YEAR")
                         <*> argument auto (metavar "PAGECOUNT"))
             (progDesc "Add book. Args: AUTHORS TITLE CITY PUBLISHER YEAR PAGECOUNT"))))
      <*> optional (subparser
           (command "addArticle"
             (info ( (,,,,,,) <$> argument str (metavar "AUTHORS")
                         <*> argument str (metavar "TITLE")
                         <*> argument str (metavar "JOURNAL")
                         <*> argument auto (metavar "YEAR")
                         <*> argument auto (metavar "JOURNALNUMBER")
                         <*> argument auto (metavar "STARTPAGE")
                         <*> argument auto (metavar "ENDPAGE"))
             (progDesc "Add article. Args: AUTHORS TITLE JOURNAL YEAR JOURNALNUMBER STARTPAGE ENDPAGE"))))
      <*> optional (subparser
          (command "addThesis"
            (info ( (,,,,,,) <$> argument str (metavar "AUTHORS")
                        <*> argument str (metavar "TITLE")
                        <*> argument str (metavar "CONFERENCE")
                        <*> argument str (metavar "CITY")
                        <*> argument auto (metavar "YEAR")
                        <*> argument auto (metavar "STARTPAGE")
                        <*> argument auto (metavar "ENDPAGE"))
            (progDesc "Add thesis. Args:AUTHORS TITLE CONFERENCE CITY YEAR STARTPAGE ENDPAGE"))))
      <*> optional (option auto ( long "update" <> short 'u'
                                <> metavar "ID"
                                <> help "Update entry with specified ID"))
      <*> switch ( long "silent"
                <> help "Silent mode, do not print to console" )
      <*> switch ( long "textOutput"
                <> help "Output result to text file" )
      <*> switch ( long "htmlOutput"
                <> help "Output result to HTML file" )

            
main :: IO ()
main = do
  opts <- execParser optsParser
  executeFunctions opts

executeFunctions :: Options -> IO ()
executeFunctions opts = do
  when (textOutput opts) $ writeFile "output.txt" ""
  when (htmlOutput opts) $ writeFile "output.html" ""
  maybe (return ()) (\id -> deletePublication opts id) (delete opts)
  maybe (return ()) (\name -> pubType opts name) (pubTypeName opts)
  maybe (return ()) (\author -> pubByAuthor opts author) (pubByAuthorName opts)
  maybe (return ()) (\author -> soloPubByAuthor opts author) (soloPubByAuthorName opts)
  when (pubTypeStats opts) (pubTypeStatistics opts)
  when (allPubJourConf opts) (getAllPubJourConf opts)
  maybe (return ()) (\(author, title, city, publisher, year, pageCount) -> addBook opts (parseAuthors author) title city publisher year pageCount) (addB opts)
  maybe (return ()) (\(author, title, journal, year, journalNum, startPage, endPage) -> addArticle opts (parseAuthors author) title journal year journalNum (startPage, endPage)) (addA opts)
  maybe (return ()) (\(author, title, conference, city, year, startPage, endPage) -> addThesis opts (parseAuthors author) title conference city year (startPage, endPage)) (addT opts)
  maybe (return ()) (\id -> updateCommand opts id) (update opts)
  when (readPub opts) (readPublications opts)

  
optsParser :: ParserInfo Options
optsParser = info (options <**> helper)
  ( fullDesc
  <> progDesc "This program demonstrates flags parsing with optparse-applicative"
  <> header "optparse-applicative example" )

  -- Парсер для авторів
parseAuthors :: String -> [String]
parseAuthors input = splitOn ", " input

