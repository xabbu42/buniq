{-# LANGUAGE DeriveDataTypeable #-}

import Data.Generics hiding (GT)
import System.IO
import Control.Monad.ST.Lazy
import Control.Monad
import Data.BloomFilter
import Data.BloomFilter.Easy (suggestSizing)
import Data.BloomFilter.Hash (cheapHashes)
import qualified System.Console.CmdArgs as CMD
import System.Console.CmdArgs ((&=))

data Args = Args { doubles          :: Bool
                 , caseinsensitive  :: Bool
                 , ignoreFields     :: Int
                 , ignoreCharacters :: Int
                 , verbose          :: Bool
                 , inputFile        :: String
                 , outputFile       :: String
                 }
          deriving (Show, Data, Typeable)

defaults = Args { doubles = False
                            &= CMD.name "d"
                            &= CMD.help "Only output lines that are repeated in the input."
                , caseinsensitive = False
                                    &= CMD.name "i"
                                    &= CMD.help "Case insensitive comparison of lines."
                , ignoreFields = 0
                                 &= CMD.name "f"
                                 &= CMD.help "Ignore the first num fields in each input line when doing comparisons. A field is a string of non-blank characters separated from adjacent fields by blanks.  Field numbers are one based, i.e., the first field is field one."
                , ignoreCharacters = 0
                                     &= CMD.name "c"
                                     &= CMD.help "Ignore the first chars characters in each input line when doing comparisons.  If specified in conjunction with the -f option, the first chars characters after the first num fields will be ignored.  Character numbers are one based, i.e., the first character is character one."
                , verbose = False
                            &= CMD.name "v"
                            &= CMD.help "Verbose output to stderror."
                , inputFile  = "" &= CMD.argPos 0 &= CMD.typFile &= CMD.opt ""
                , outputFile = "" &= CMD.argPos 1 &= CMD.typFile &= CMD.opt ""
                }
           &= CMD.program "buniq"
           &= CMD.details ["Implements a subset of the functionality of uniq on"
                           ++ " unsorted input using bloom filters."]

main :: IO ()
main = do
  args <- CMD.cmdArgs defaults
  content <- if (inputFile args /= "") then readFile (inputFile args) else getContents
  when (verbose args)
    $ hPutStrLn stderr $ "size: " ++ show (size `div` (8 * 1024)) ++ " hashnum: " ++ show hashnum
  mapM_ putStrLn (filter args $ lines content)
  where
    (size, hashnum)  = suggestSizing (8 * 1024 * 1024) 0.0001
    hashes           = cheapHashes hashnum
    filter args      = if doubles args
                       then bloomFilter not hashes size . bloomFilter id hashes size
                       else bloomFilter not hashes size

bloomFilter :: (Bool -> Bool) -> (String -> [Hash]) -> Int -> [String] -> [String]
bloomFilter func hashes size xs = runST $ do
  bloom <- strictToLazyST $ newMB hashes size
  filterM (filter bloom) xs
  where
    filter bloom x = strictToLazyST $ do
      known <- x `elemMB` bloom
      insertMB bloom x
      return $ func known
