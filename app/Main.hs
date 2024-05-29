module Main where

import qualified MyLib
import qualified CLI
import Text.Read (readMaybe)
import System.IO
import Data.Maybe (mapMaybe)
import Control.Exception (evaluate)

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

readPublications :: IO ()
readPublications  = do
    publications <- readPublicationsFromFile
    MyLib.printPublications publications

addPublication :: MyLib.Publication -> IO ()
addPublication pub = do
    publications <- readPublicationsFromFile
    let updatedPublications = publications <> MyLib.Publications [pub]
    writePublicationsToFile updatedPublications
    putStrLn "Publication added successfully"

deletePublication :: Int -> IO ()
deletePublication idx = do
    MyLib.Publications publications <- readPublicationsFromFile
    if idx < 0 || idx >= length publications
        then putStrLn "Invalid index"
        else do
            let updatedPublications = take idx publications ++ drop (idx + 1) publications
            writePublicationsToFile (MyLib.Publications updatedPublications)
            putStrLn "Publication deleted successfully"

updatePublication :: Int -> MyLib.Publication -> IO ()
updatePublication idx pub = do
    MyLib.Publications publications <- readPublicationsFromFile
    if idx < 0 || idx >= length publications
        then putStrLn "Invalid index"
        else do
            let updatedPublications = take idx publications ++ [pub] ++ drop (idx + 1) publications
            writePublicationsToFile (MyLib.Publications updatedPublications)
            putStrLn "Publication updated successfully"

pubType :: String -> IO ()
pubType name = do
   publications <- readPublicationsFromFile
   putStrLn $ "\nPublication type for " ++ name ++ ":"
   case MyLib.publicationType name publications  of
        Just pubType -> putStrLn pubType
        Nothing -> putStrLn "Publication not found"

pubByAuthor :: String -> IO()
pubByAuthor author = do
    publications <- readPublicationsFromFile
    putStrLn $ "\nPublications by " ++ author ++ ":"
    MyLib.printPublications $ MyLib.findPublicationsByAuthor publications author

soloPubByAuthor :: String -> IO()
soloPubByAuthor author = do
    publications <- readPublicationsFromFile
    putStrLn $ "\nSolo publications by " ++ author ++ ":"
    MyLib.printPublications $ MyLib.findSoloPublicationsByAuthor publications author

pubTypeStatistics :: IO()
pubTypeStatistics = do
    publications <- readPublicationsFromFile
    putStrLn "\nPublication type statistics:"
    print $ MyLib.publicationTypeStatistics publications

getAllPubJourConf :: IO()
getAllPubJourConf = do
    publications <- readPublicationsFromFile
    putStrLn "\nUnique publishers, journals, and conferences:"
    let categories = MyLib.getAllPublishersJournalsConferences publications
    mapM_ (\(category, items) -> do
        putStrLn category
        mapM_ putStrLn items
        putStrLn ""
        ) categories

        
main :: IO ()
main = do
    let newBook = MyLib.Book ["John Doe"] "Introduction to Haskell" "New York" "ABC Publishing" 2024 400
    addPublication newBook
    readPublications
    deletePublication 10
    updatePublication 2 (MyLib.Book ["John Doe"] "Introduction to Haskell" "New York" "ABC Publishing" 2024 400)
    pubType "Introduction to Haskell"
    pubByAuthor "John Doe"
    soloPubByAuthor "John Doe"
    pubTypeStatistics
    getAllPubJourConf

    