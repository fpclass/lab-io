--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Input & Output                                                        --
--------------------------------------------------------------------------------

module Lab where 

--------------------------------------------------------------------------------

import Control.Monad

import Data.Bifunctor

-- We import hGetContents from System.IO.Strict (from the `strict` package) 
-- instead of the implementation in System.IO since the latter is lazy which
-- tends to cause problems when performing I/O.
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)

--------------------------------------------------------------------------------

-- | 'extractFirstLine' @inHandle outHandle@ reads the first line from 
-- @inHandle@ and writes it to @outHandle@.
extractFirstLine :: Handle -> Handle -> IO ()
-- extractFirstLine inHandle outHandle = do
--     line <- hGetLine inHandle
--     hPutStr outHandle line
extractFirstLine inHandle outHandle = 
    hGetLine inHandle >>= hPutStr outHandle

-- | 'replicateFirstLine' @inHandle outHandle@ reads the first line from
-- @inHandle@ and writes it to @outHandle@ five times.
replicateFirstLine :: Handle -> Handle -> IO ()
-- replicateFirstLine inHandle outHandle = do
--     line <- hGetLine inHandle
--     sequence_ $ replicate 5 (hPutStrLn outHandle line)
-- replicateFirstLine inHandle outHandle = do
--     line <- hGetLine inHandle
--     void $ replicateM 5 (hPutStrLn outHandle line)
-- replicateFirstLine inHandle outHandle = do
--     line <- hGetLine inHandle
--     replicateM_ 5 (hPutStrLn outHandle line) 
replicateFirstLine inHandle outHandle =
    hGetLine inHandle >>= replicateM_ 5 . hPutStrLn outHandle 

-- | 'reverseFile' @inHandle outHandle@ reads all lines from @inHandle@ and
-- writes them to @outHandle@ in reverse order.
reverseFile :: Handle -> Handle -> IO ()
reverseFile inHandle outHandle =
    hGetContents inHandle >>= hPutStr outHandle . unlines . reverse . lines 

--------------------------------------------------------------------------------

-- | 'readKeyValuePair' @line@ parses a string of form @"key=value"@ into a pair
-- of the form @("key", "value")@.
readKeyValuePair :: String -> (String, String)
readKeyValuePair = fmap tail . span (/='=') 

-- | 'readDictionary' @inHandle@ reads all lines from @inHandle@ and parses each
-- of them using 'readKeyValuePair'.
readDictionary :: Handle -> IO [(String, String)]
readDictionary = fmap (map readKeyValuePair . lines) . hGetContents  

grandduck :: [(String, String)] -> String -> Maybe String
grandduck rels name = lookup name rels >>= flip lookup rels 

-- | 'writeGrandDucks' @dictPath inPath outPath@ reads the file at @dictPath@ 
-- using 'readDictionary' and, for each line in the file at @inPath@, 
-- determines the grandduck of the named duck using the data from the file
-- at @dictPath@. If a grandduck is found, its name is written to a file at 
-- @outPath@. Otherwise, a dash is written to the file instead.
writeGrandDucks :: FilePath -> FilePath -> FilePath -> IO ()
writeGrandDucks dictPath inPath outPath =
    withFile dictPath ReadMode $ \dictHandle ->
    withFile inPath ReadMode $ \inHandle ->
    withFile outPath WriteMode $ \outHandle -> do 
        dict <- readDictionary dictHandle
        ducks <- lines <$> hGetContents inHandle
        forM_ ducks $ \duck -> case grandduck dict duck of 
            Nothing -> hPutStrLn outHandle "-"
            Just gd -> hPutStrLn outHandle gd
    
-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- withFile fp mode cont = do 
--     h <- openFile fp mode
--     r <- cont h
--     closeFile h
--     pure r

--------------------------------------------------------------------------------
