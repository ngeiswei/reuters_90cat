#!/usr/bin/runhaskell

import qualified Data.MultiSet as MSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO
import Data.List
import NLP.Stemmer (stem, Stemmer(English))
import Data.CSV (genCsvFile)
import Data.Char (toLower)
import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath (pathSeparator)
import Control.Exception (assert)
import Text.Regex.Posix ((=~))

-- Build dataset from Reuters 90 cat. Give in argument the directory
-- where has been unpacked and produces 2 files, train and test sets
-- in CSV format. Each row corresponds to a message, and each column
-- correspond to a word or a category. Categories have the syntax
-- __category__ in order to distinguish them from words.

main :: IO ()
main = do
  [reutersDir] <- getArgs
  buildTrainTestCSVFiles reutersDir

buildTrainTestCSVFiles :: FilePath -> IO ()
buildTrainTestCSVFiles filePath = do 
  reuters_dir <- getDirectoryContents filePath
  let test_dir = "test"
      train_dir = "training"
      reuters_test_dir = filePath ++ [pathSeparator] ++ test_dir
      reuters_train_dir = filePath ++ [pathSeparator] ++ train_dir
  -- let my_assert_result = assert ((elem test_dir reuters_dir) and (elem train_dir reuters_dir)) ""
  test_words <- stemFile reuters_test_dir
  train_words <- stemFile reuters_train_dir
  putStrLn ("test words = " ++ (show test_words))
  putStrLn ("train words = " ++ (show train_words))

-- Take a file (a normal file or a directory). If the file is not a
-- directory, then return a set of stemmed words in that
-- file. Otherwise (if the file is a directory), call this function on
-- all files of that direcory and build a set stemmed words.
stemFile :: FilePath -> IO (Set.Set String)
stemFile filePath = do
  putStrLn ("filePath = " ++ filePath)
  isDir <- doesDirectoryExist filePath
  if isDir then do              -- Recursive case
    children <- getDirectoryContents filePath
    let children_wc = delete (".." :: FilePath) (delete ("." :: FilePath) children)
        mkfullpath child = filePath ++ [pathSeparator] ++ child
    stemmedSets <- sequence (map stemFile (map mkfullpath children_wc))
    return (Set.unions stemmedSets)
  else do                       -- Base case
    contents <- readFile filePath
    let stmWords = stemMsg contents
    putStrLn ("stmWords = " ++ (show (length stmWords)))
    return (Set.fromList stmWords)

-- TODO
-- buildWordsCat2Msg

-- Given
--
-- 1. a list of all words
--
-- 2. a mapping from category to a list of messages (specifically a
-- pair (message ID, Message)) 
--
-- Return a CSV file, according to the format defined in the comment
-- above. More specifically it is a list of rows. Each row is a list
-- of Strings. The first row corresponds to the header, the other rows
-- to the content.
buildCSV :: [String] -> Map.Map String [(String, String)] -> [[String]]
buildCSV wordList cat2Msg = header : csvrows
  where header = ["message_id"] ++ (Map.keys cat2Msg) ++ wordList
        maprows = concat (myFoldMapWithKey buildRows cat2Msg)
        csvrows = map (maprow2csvrow header) maprows

myFoldMapWithKey :: (k -> a -> b) -> Map.Map k a -> [b]
myFoldMapWithKey f m = Map.foldrWithKey (\k a l -> (f k a) : l) [] m

maprow2csvrow :: [String] -> Map.Map String String -> [String]
maprow2csvrow header maprow = map (\k -> Map.findWithDefault "0" k maprow) header

-- Given a category and pair (message ID, Message) return a map
-- associating category to "1", "message_id" to the message ID, and
-- each word to it's number of occurences.
--
-- TODO: don't redo the stemming
buildRow :: String -> (String, String) -> Map.Map String String
buildRow category (message_id, message) =
  Map.fromList ([("message_id", message_id), (category, "1")] ++
               (ms2plist (MSet.fromList (stemMsg message))))

-- Given a category and a list of pairs (message ID, Message) return a
-- list of maps as buildRow does.
buildRows :: String -> [(String, String)] -> [Map.Map String String]
buildRows category (x:l) = (buildRow category x) : (buildRows category l)
buildRows category [] = []

-- Convert a multiset into a list of pairs (key, count) replacing the
-- count by a string of the count.
ms2plist :: MSet.MultiSet String -> [(String, String)]
ms2plist ms = MSet.foldOccur (\key count pl -> (key, show count) : pl) [] ms

-- Takes a message, stem all words, remove the junk and put it to
-- lower case, and return that list of words (including duplicates).
stemMsg :: String -> [String]
stemMsg = concat . map (stemWords . lowerWords) . lines

-- Stem a list of words and only retain the alpha words
stemWords :: [String] -> [String]
stemWords ws = filter isAlphaWord (map (stem English) ws)

-- Like words but output everything in lower case
lowerWords :: String -> [String]
lowerWords = words . map toLower

isAlphaWord :: String -> Bool
isAlphaWord w = w =~ "^[[:alpha:]]+$"
