--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Input & Output                                                        --
--------------------------------------------------------------------------------

module Lab where 

--------------------------------------------------------------------------------

import Control.Monad

-- We import hGetContents from System.IO.Strict (from the `strict` package) 
-- instead of the implementation in System.IO since the latter is lazy which
-- tends to cause problems when performing I/O.
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)

--------------------------------------------------------------------------------

-- | 'extractFirstLine' @inHandle outHandle@ reads the first line from 
-- @inHandle@ and writes it to @outHandle@.
extractFirstLine :: Handle -> Handle -> IO ()
extractFirstLine inHandle outHandle = undefined

-- | 'replicateFirstLine' @inHandle outHandle@ reads the first line from
-- @inHandle@ and writes it to @outHandle@ five times.
replicateFirstLine :: Handle -> Handle -> IO ()
replicateFirstLine inHandle outHandle = undefined

-- | 'reverseFile' @inHandle outHandle@ reads all lines from @inHandle@ and
-- writes them to @outHandle@ in reverse order.
reverseFile :: Handle -> Handle -> IO ()
reverseFile inHandle outHandle = undefined

--------------------------------------------------------------------------------

-- | 'readKeyValuePair' @line@ parses a string of form @"key=value"@ into a pair
-- of the form @("key", "value")@.
readKeyValuePair :: String -> (String, String)
readKeyValuePair input = undefined

-- | 'readDictionary' @inHandle@ reads all lines from @inHandle@ and parses each
-- of them using 'readKeyValuePair'.
readDictionary :: Handle -> IO [(String, String)]
readDictionary inHandle = undefined

-- | 'writeGrandDucks' @dictPath inPath outPath@ reads the file at @dictPath@ 
-- using 'readDictionary' and, for each line in the file at @inPath@, 
-- determines the grandduck of the named duck using the data from the file
-- at @dictPath@. If a grandduck is found, its name is written to a file at 
-- @outPath@. Otherwise, a dash is written to the file instead.
writeGrandDucks :: FilePath -> FilePath -> FilePath -> IO ()
writeGrandDucks dictPath inPath outPath = undefined

--------------------------------------------------------------------------------
