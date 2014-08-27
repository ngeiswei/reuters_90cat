#!/usr/bin/runhaskell

import qualified Data.MultiSet as MSet
import qualified Data.Map as Map
import qualified Data.MultiMap as MMap
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Data.Functor
import Data.CSV (genCsvFile)
import Data.Char (toLower)

import Control.Exception (assert)
import Control.Monad

import System.IO
import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath (addTrailingPathSeparator, takeFileName)

import Text.Regex.Posix ((=~))

import NLP.Stemmer (stem, Stemmer(English))

-- Build dataset from Reuters 90 cat. Give in argument the directory
-- where has been unpacked and produces 2 files, train and test sets
-- in CSV format. Each row corresponds to a message, and each column
-- correspond to a word or a category. Categories have the syntax
-- __category__ in order to distinguish them from words.

main :: IO ()
main = do
  [reutersDir] <- getArgs
  mkTrainTestCSVFiles reutersDir

-- Like mapM but return a Map mapping input to output, instead of list
-- of outputs (suggested by lfairy on #haskell)
lfairyM :: (Ord k, Functor f, Monad f) => (k -> f a) -> [k] -> f (Map.Map k a)
lfairyM f = fmap Map.fromList . mapM (\x -> (,) x <$> f x)

-- Flipped version of lfairyM
flfairyM :: (Ord k, Functor f, Monad f) => [k] -> (k -> f a) -> f (Map.Map k a)
flfairyM = flip lfairyM

mkTrainTestCSVFiles :: FilePath -> IO ()
mkTrainTestCSVFiles filePath = do
  let samples = ["training", "test"]
      mkSmpPathPair smp = (smp, (addTrailingPathSeparator filePath) ++ smp)
      smp2reutersDir = Map.fromList (map mkSmpPathPair samples)
      -- Unsafe Map lookup
      get element container = fromJust (Map.lookup element container)

  smp2Cat2Words <- flfairyM samples $ \smp -> do
    putStrLn ("Build multimap from category to words " ++ "(" ++ smp ++ ")...")
    cat2words <- mkCat2Words (get smp smp2reutersDir)
    return cat2words
  putStrLn "...done"

  putStrLn "Select words..."
  let selWords = selectWords (get "training" smp2Cat2Words)

  smp2CSV <- flfairyM samples $ \smp -> do
    putStrLn ("Build CSV content " ++ "(" ++ smp ++ ")...")
    return (mkCSV selWords (get smp smp2Cat2Words))
  putStrLn "...done"

  smp2CSV <- flfairyM samples $ \smp -> do
    putStrLn ("Gen CSV string " ++ "(" ++ smp ++ ")...")
    return (genCsvFile (get smp smp2CSV))
  putStrLn "...done"

  -- putStrLn "Write CSV file (test)..."
  -- writeFile "test.csv" testCSVContent
  -- putStrLn "...done"

  -- putStrLn "Write CSV file (train)..."
  -- writeFile "train.csv" trainCSVContent
  -- putStrLn "...done"

-- Structure to represent the list of words occurences associated to
-- each category. A multimap is used so a category can have multiple
-- message. A multiset is used so that the word count, not just the
-- existence, is stored.
type Cat2Words = MMap.MultiMap String (MSet.MultiSet String)

-- Take a test or train directory containing as subdirectories the
-- categories, which contains the messages. Return a multi-mapping of
-- category to a multiset of stemmed words. We use a multimap so that
-- a category can contain multiple messages, and we use a multiset so
-- that words in a message are counted multiple times.
mkCat2Words :: FilePath -> IO (Cat2Words)
mkCat2Words filePath = do
  children <- getDirectoryContents filePath
  let categories = delCPDirs children
      mkfullpath child = (addTrailingPathSeparator filePath) ++ child
  catStemmedSets <- sequence (map stemCategory (map mkfullpath categories))
  return (MMap.fromList (concat catStemmedSets))

-- Remove current or parent directories from a list of directories
delCPDirs dirs = delete (".." :: FilePath) (delete ("." :: FilePath) dirs)

-- Take a category directory and return a list of pairs (category,
-- multisets of stemmed words). Each element of the list correspond to
-- a message.
stemCategory :: FilePath -> IO ([(String, MSet.MultiSet String)])
stemCategory catFilePath = do
  children <- getDirectoryContents catFilePath
  let messages = delCPDirs children
      mkfullpath child = (addTrailingPathSeparator catFilePath) ++ child
      category = takeFileName catFilePath
      msgFullPaths = map mkfullpath messages
      catStemFile filePath = do
        words <- stemFile filePath
        return (category, words)
  sequence (map catStemFile msgFullPaths)

-- Take a non-directory file and return a multiset of stemmed words in
-- that file
stemFile :: FilePath -> IO (MSet.MultiSet String)
stemFile filePath = do
  contents <- readFile filePath
  let stmWords = stemMsg contents
  -- trick to actually read the file
  putStrLn ("stmWords = " ++ (show (length stmWords)))
  return (MSet.fromList stmWords)

-- Given a Cat2Words strucure (provided from train to avoid having
-- test information snooping in the train data), return a set of words
-- used for training (and test). At this point the filter just
-- includes words with total occurance above a certain threshold.
wordCountThreshold :: MSet.Occur
wordCountThreshold = 10
selectWords :: Cat2Words -> Set.Set String
selectWords cat2words = MSet.foldOccur op Set.empty allwords
  where allwords = MSet.unions (concat (MMap.elems cat2words))
        op word count selWords = if count > wordCountThreshold then
                                   Set.insert word selWords
                                 else selWords

-- Given a set of words (obtained from looking at the train data only)
-- and Cat2Words structure, return a CSV file (according to the format
-- defined in the comment above). More specifically it is a list of
-- rows. Each row is a list of Strings. The first row corresponds to
-- the header, the other rows to the content.
mkCSV :: Set.Set String -> Cat2Words -> [[String]]
mkCSV selWords cat2words = header : csvrows
  where allCats = MMap.keys cat2words
        header = (map addCatSig allCats) ++ (Set.toList selWords)
        op category words rows = mkCSVRow category words header : rows
        csvrows = MMap.foldrWithKey op [] cat2words

-- Add category signature (in order to distiguish them from text
-- words). The signature is __category__.
addCatSig :: String -> String
addCatSig cat = "__" ++ cat ++ "__"

-- Check whether a string has the category signature.
isCat :: String -> Bool
isCat str = str =~ "^__.+__$"

-- Given a category, a multiset of stemmed words, the header
-- (categories stemmed words) return a list of integers. If the
-- category matches the category in the header, return "1", "0"
-- otherwise. If the word matches the word in the header, return its
-- count (according to the multiset), "0" otherwise.
mkCSVRow :: String -> (MSet.MultiSet String) -> [String] -> [String]
mkCSVRow category words header = map mkvalue header
  where catSig = addCatSig category
        mkvalue k | (k == catSig) = "1"
                  | otherwise = show (MSet.occur k words)

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
