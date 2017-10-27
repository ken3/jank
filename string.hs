#!/usr/bin/env runhaskell

-- 文字列の扱い

import System.Environment
import Data.List

-- cat
cat :: IO ()
cat = do cs <- getContents
         putStr cs

-- countline
countline :: IO ()
countline = do cs <- getContents
               print $ length $ lines cs

-- head
myHead :: IO ()
myHead = do cs <- getContents
            putStr $ firstNLines 10 cs
firstNLines n cs = unlines $ take n $ lines cs

-- tail
tail :: IO ()
tail = do cs <- getContents
          putStr $ lastNLines 10 cs
lastNLines n ss = reverse $ take n $ reverse ss

-- expand
tabStop = 8
expand :: IO ()
expand = do cs <- getContents
            putStr $ expand' cs

expand' :: String -> String
expand' cs = concatMap expandTab cs

expandTab :: Char -> String
expandTab '\t' = replicate tabStop ' '
expandTab c    = [c]

-- echo
echo :: IO ()
echo = do args <- getArgs
          putStrLn $ unwords args

-- fgrep
fgrep :: IO ()
fgrep = do args <- getArgs
           cs <- getContents
           putStr $ fgrep' (head args) cs

fgrep' :: String -> String -> String
fgrep' pattern cs = unlines $ filter match $ lines cs
  where match :: String -> Bool
        match line = any prefixp $ tails line
        prefixp :: String -> Bool
        prefixp line = pattern `isPrefixOf` line

