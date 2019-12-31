--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Input & Output                                                        --
--------------------------------------------------------------------------------

import Control.Monad.Extra

import Test.Tasty
import Test.Tasty.HUnit

import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import System.Directory

import qualified Lab as L

--------------------------------------------------------------------------------

assertFileEqual :: FilePath -> FilePath -> Assertion 
assertFileEqual fp0 fp1 = 
   withFile fp0 ReadMode $ \h0 ->
   withFile fp1 ReadMode $ \h1 -> do 
      xs <- hGetContents h0 
      ys <- hGetContents h1 
      assertEqual "The output produced does not match what was expected:" ys xs

tests :: TestTree 
tests = testGroup "" 
 [
    testCase "extractFirstLine" $ do 
      let outPath = "out/extractFirstLine.txt"

      -- if there is already an output file, remove it
      whenM (doesFileExist outPath) $ removeFile outPath

      -- call extractFirstLine with appropriate handles
      withFile "data/cake.txt" ReadMode $ \inHandle ->
         withFile outPath WriteMode $ \outHandle -> 
         L.extractFirstLine inHandle outHandle

      -- check that the output is as expected
      assertFileEqual outPath "data/extractFirstLine.txt"
   , testCase "replicateFirstLine" $ do 
      let outPath = "out/replicateFirstLine.txt"

      -- if there is already an output file, remove it
      whenM (doesFileExist outPath) $ removeFile outPath

      -- call replicateFirstLine with appropriate handles
      withFile "data/cake.txt" ReadMode $ \inHandle ->
         withFile outPath WriteMode $ \outHandle -> 
         L.replicateFirstLine inHandle outHandle

      -- check that the output is as expected
      assertFileEqual outPath "data/replicateFirstLine.txt"
   , testCase "reverseFile" $ do 
      let outPath = "out/reverseFile.txt"

      -- if there is already an output file, remove it
      whenM (doesFileExist outPath) $ removeFile outPath

      -- call reverseFile with appropriate handles
      withFile "data/cake.txt" ReadMode $ \inHandle ->
         withFile outPath WriteMode $ \outHandle -> 
         L.reverseFile inHandle outHandle

      -- check that the output is as expected
      assertFileEqual outPath "data/reverseFile.txt"
   , testCase "readKeyValuePair" $ assertEqual ""
      (L.readKeyValuePair "Duck Vader=Grand duckster")
      ("Duck Vader", "Grand duckster")
   , testCase "readDictionary" $ do 
      -- call readDictionary with an appropriat handle
      withFile "data/duckily.txt" ReadMode $ \inHandle -> do
         dict <- L.readDictionary inHandle

         -- check that the dictionary is as expected
         let expectedDict = 
               [ ("Grandduck", "Grand duckster")
               , ("Baby duck", "Parent duck") 
               , ("Duckling", "Older duckling") 
               , ("Parent duck", "Grandduck") 
               , ("Duck Vader", "Grand duckster") 
               ]

         assertEqual "" expectedDict dict
   , testCase "writeGrandDucks" $ do 
      let outPath = "out/writeGrandDucks.txt"

      -- if there is already an output file, remove it
      whenM (doesFileExist outPath) $ removeFile outPath

      -- call writeGrandDucks with appropriate paths
      L.writeGrandDucks "data/duckily.txt" "data/ducks.txt" outPath

      -- check that the output is as expected
      assertFileEqual outPath "data/writeGrandDucks.txt"
 ]

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

--------------------------------------------------------------------------------
