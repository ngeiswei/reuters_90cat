#!/usr/bin/runhaskell

import qualified Data.MultiSet as MSet
import qualified Data.Map as Map
import qualified Data.MultiMap as MMap
import qualified Data.Set as Set

import Data.Maybe (fromJust)
import Data.Functor ((<$>))
import Data.List (map, delete, concat)
import Data.Text (Text, pack, unpack, concat, lines, words, toLower)
import Data.Text.IO (readFile)

import Control.Exception (assert)
import Control.Monad (forM_)

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
  putStrLn ("...selected " ++ (show (Set.size selWords)) ++ " words")

  smp2CSVContents <- flfairyM samples $ \smp -> do
    putStrLn ("Build CSV content " ++ "(" ++ smp ++ ")...")
    return (mkCSV selWords (get smp smp2Cat2Words))
  putStrLn "...done"

  forM_ samples $ \smp -> do
    putStrLn ("Write CSV file " ++ "(" ++ smp ++ ")...")
    writeCSVFile (smp ++ ".csv") (get smp smp2CSVContents)
  putStrLn "...done"

-- Structure to represent the list of words occurences associated to
-- each category. A multimap is used so a category can have multiple
-- message. A multiset is used so that the word count, not just the
-- existence, is stored.
type Cat2Words = MMap.MultiMap Text (MSet.MultiSet Text)

writeCSVFile :: FilePath -> [[Text]] -> IO ()
writeCSVFile filePath csv = do
  putStrLn "TODO: write CSV file"

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
  return (MMap.fromList (Data.List.concat catStemmedSets))

-- Remove current or parent directories from a list of directories
delCPDirs dirs = delete (".." :: FilePath) (delete ("." :: FilePath) dirs)

-- Take a category directory and return a list of pairs (category,
-- multisets of stemmed words). Each element of the list correspond to
-- a message.
stemCategory :: FilePath -> IO ([(Text, MSet.MultiSet Text)])
stemCategory catFilePath = do
  children <- getDirectoryContents catFilePath
  let messages = delCPDirs children
      mkfullpath child = (addTrailingPathSeparator catFilePath) ++ child
      category = pack (takeFileName catFilePath)
      msgFullPaths = map mkfullpath messages
      catStemFile filePath = do
        words <- stemFile filePath
        return (category, words)
  sequence (map catStemFile msgFullPaths)

-- Take a non-directory file and return a multiset of stemmed words in
-- that file
stemFile :: FilePath -> IO (MSet.MultiSet Text)
stemFile filePath = do
  contents <- Data.Text.IO.readFile filePath
  return (MSet.fromList (stemMsg contents))

-- Given a Cat2Words strucure (provided from train to avoid having
-- test information snooping in the train data), return a set of words
-- used for training (and test). At this point the filter just
-- includes words with total occurance above a certain threshold.
wordCountThreshold :: MSet.Occur
wordCountThreshold = 1000
selectWords :: Cat2Words -> Set.Set Text
selectWords cat2words = MSet.foldOccur op Set.empty allwords
  where allwords = MSet.unions (Data.List.concat (MMap.elems cat2words))
        op word count selWords = if count > wordCountThreshold then
                                   Set.insert word selWords
                                 else selWords

-- Given a set of words (obtained from looking at the train data only)
-- and Cat2Words structure, return a CSV file (according to the format
-- defined in the comment above). More specifically it is a list of
-- rows. Each row is a list of Strings. The first row corresponds to
-- the header, the other rows to the content.
mkCSV :: Set.Set Text -> Cat2Words -> [[Text]]
mkCSV selWords cat2words = header : csvrows
  where allCats = MMap.keys cat2words
        header = (map addCatSig allCats) ++ (Set.toList selWords)
        op category words rows = mkCSVRow category words header : rows
        csvrows = MMap.foldrWithKey op [] cat2words

-- Add category signature (in order to distiguish them from text
-- words). The signature is __category__.
catSig :: Text
catSig = pack "__"
addCatSig :: Text -> Text
addCatSig cat = Data.Text.concat [catSig, cat, catSig]

-- Check whether a string has the category signature.
isCat :: Text -> Bool
isCat str = (unpack str) =~ "^__.+__$"

-- Given a category, a multiset of stemmed words, the header
-- (categories stemmed words) return a list of integers. If the
-- category matches the category in the header, return "1", "0"
-- otherwise. If the word matches the word in the header, return its
-- count (according to the multiset), "0" otherwise.
mkCSVRow :: Text -> (MSet.MultiSet Text) -> [Text] -> [Text]
mkCSVRow category words header = map mkvalue header
  where catSig = addCatSig category
        mkvalue k | (k == catSig) = pack "1"
                  | otherwise = pack (show (MSet.occur k words))

-- Takes a message, stem all words, remove the junk and put it to
-- lower case, and return that list of words (including duplicates).
stemMsg :: Text -> [Text]
stemMsg = Data.List.concat . map (stemWords . lowerWords) . Data.Text.lines

-- Stem a list of words and only retain the alpha words
stemWords :: [Text] -> [Text]
stemWords ws = filter isAlphaWord (map bstem ws)

-- Call stem on a ByteString, do the necessary convertion
bstem :: Text -> Text
bstem bsw = pack (stem English (unpack bsw))

-- Like words but output everything in lower case
lowerWords :: Text -> [Text]
lowerWords = Data.Text.words . Data.Text.toLower

isAlphaWord :: Text -> Bool
isAlphaWord w = (unpack w) =~ "^[[:alpha:]]+$"
