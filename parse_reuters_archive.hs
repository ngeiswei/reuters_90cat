#!/usr/bin/runhaskell

import qualified Data.MultiSet as MS
import qualified Data.Map as DM
import NLP.Stemmer (stem, Stemmer(English))
import Data.CSV (genCsvFile)
import Data.Char (toLower)
import System.Environment (getArgs)
import System.Directory (getDirectoryContents)
import Text.Regex.Posix ((=~))

-- Build dataset from Reuters 90 cat. Give in argument the directory
-- where has been unpacked and produces 2 files, train and test sets
-- in CSV format. Each row corresponds to a message, and each column
-- correspond to a word or a category.

main :: IO ()
main = do
  [reutersDir] <- getArgs
  buildTrainTestCSVFiles reutersDir

buildTrainTestCSVFiles :: FilePath -> IO ()
buildTrainTestCSVFiles filePath = do 
  dirContent <- getDirectoryContents filePath
  putStrLn (show dirContent)

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
buildCSV :: [String] -> DM.Map String [(String, String)] -> [[String]]
buildCSV wordList cat2Messages = header : csvrows
  where header = ["message_id"] ++ (DM.keys cat2Messages) ++ wordList
        maprows = concat (myFoldMapWithKey buildRows cat2Messages)
        csvrows = map (maprow2csvrow header) maprows

myFoldMapWithKey :: (k -> a -> b) -> DM.Map k a -> [b]
myFoldMapWithKey f m = DM.foldrWithKey (\k a l -> (f k a) : l) [] m

maprow2csvrow :: [String] -> DM.Map String String -> [String]
maprow2csvrow header maprow = map (\k -> DM.findWithDefault "0" k maprow) header

-- Given a category and pair (message ID, Message) return a map
-- associating category to "1", "message_id" to the message ID, and
-- each word to it's number of occurences.
buildRow :: String -> (String, String) -> DM.Map String String
buildRow category (message_id, message) =
  DM.fromList ([("message_id", message_id), (category, "1")] ++
               (ms2plist (MS.fromList (stemMessage message))))

-- Given a category and a list of pairs (message ID, Message) return a
-- list of maps as buildRow does.
buildRows :: String -> [(String, String)] -> [DM.Map String String]
buildRows category (x:l) = (buildRow category x) : (buildRows category l)
buildRows category [] = []

-- Convert a multiset into a list of pairs (key, count) replacing the
-- count by a string of the count.
ms2plist :: MS.MultiSet String -> [(String, String)]
ms2plist ms = MS.foldOccur (\key count pl -> (key, show count) : pl) [] ms

-- main = interact (show . MS.fromList . stemMessage)

-- Takes a message, stem all words, remove the junk and put it to
-- lower case, and return that list of words (including duplicates).
stemMessage :: String -> [String]
stemMessage = concat . map (stemWords . lowerWords) . lines

-- Stem a list of words and only retain the alpha words
stemWords :: [String] -> [String]
stemWords ws = filter isAlphaWord (map (stem English) ws)

-- Like words but output everything in lower case
lowerWords :: String -> [String]
lowerWords = words . map toLower

isAlphaWord :: String -> Bool
isAlphaWord w = w =~ "^[[:alpha:]]$"
