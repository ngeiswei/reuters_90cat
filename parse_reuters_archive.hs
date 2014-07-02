#!/usr/bin/runhaskell

import NLP.Stemmer
import Data.Char
import qualified Data.MultiSet as MS
import System.Environment
import System.Directory
import qualified Data.Map as Map
import Data.Monoid
import Text.Regex.Posix

-- Build dataset from Reuters 90 cat. Give in argument the directory
-- where has been unpacked and produces 2 files, train and test sets
-- in CSV format. Each row corresponds to a message, and each column
-- correspond to a word or a category.

main :: IO ()
main = do
  args <- getArgs
  let reutersDir = args !! 0 in
    doTheShit reutersDir

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
-- Return a pair (header, list of maps), i.e. the content of a CSV
-- file, according to the format defined in the comment above.
buildCSV :: [String] -> Map.Map String [(String, String)]
            -> ([String], [[String]])
buildCSV wordList cat2Messages = (header, csvrows)
  where header = ["message_id"] ++ (Map.keys cat2Messages) ++ wordList
        maprows = concat (myFoldMapWithKey buildRows cat2Messages)
        csvrows = map (maprow2csvrow header) maprows

myFoldMapWithKey :: (k -> a -> b) -> Map.Map k a -> [b]
myFoldMapWithKey f m = Map.foldrWithKey (\k a l -> (f k a) : l) [] m

maprow2csvrow :: [String] -> Map.Map String String -> [String]
maprow2csvrow header maprow = map (\k -> Map.findWithDefault "0" k maprow) header

-- Given a category and pair (message ID, Message) return a map
-- associating category to "1", "message_id" to the message ID, and
-- each word to it's number of occurences.
buildRow :: String -> (String, String) -> Map.Map String String
buildRow category (message_id, message) =
  Map.fromList ([("message_id", message_id), (category, "1")] ++
                (ms2plist (MS.fromList (stemMessage message))))

-- Given a category and a list of pairs (message ID, Message) return a
-- list of maps as buildRow does.
buildRows :: String -> [(String, String)] -> [Map.Map String String]
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
