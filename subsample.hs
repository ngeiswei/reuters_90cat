#!/usr/bin/runhaskell

-- Load a CSV file and subsample it

import Data.Text.IO (hGetLine, hPutStrLn)

import Control.Monad (unless, when, forM_)

import System.Random (mkStdGen, randoms)
import System.FilePath (dropExtension, takeExtension)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (openFile, hClose, hIsEOF, IOMode(ReadMode), IOMode(WriteMode))

main :: IO ()
main = do
  -- Capture arguments
  args <- getArgs
  prgName <- getProgName

  -- Error if not right number of arguments
  when (length args /= 3) $ do
    putStrLn ("Usage: " ++ prgName ++ " FILEPATH SUBSAMPLE_RATIO RANDOM_SEED")
    exitFailure

  -- Parse arguments, initialize random generator and open the files to
  -- read and write
  let [filePath, subsmp_ratio_str, rnd_seed_str] = args
      outputFilePath = dropExtension filePath ++
                       "_subsmp_ratio_" ++ subsmp_ratio_str ++
                       "_rnd_seed_" ++ rnd_seed_str ++
                       takeExtension filePath
      subsmp_ratio = read subsmp_ratio_str :: Float
      rnd_seed = read rnd_seed_str :: Int
      random_floats = randoms (mkStdGen rnd_seed) :: [Float]
      random_biased_bools = map ((>=) subsmp_ratio) random_floats
  hIn <- openFile filePath ReadMode
  hOut <- openFile outputFilePath WriteMode

  -- Read and write the header
  header <- hGetLine hIn
  hPutStrLn hOut header

  -- Iterate through all rows and probabilistically drop some
  -- TODO add end of file test
  forM_ random_biased_bools $ \b -> do
    isEOF <- hIsEOF hIn
    when isEOF $ do hClose hIn; hClose hOut; exitSuccess
    content <- hGetLine hIn
    when b $ hPutStrLn hOut content
