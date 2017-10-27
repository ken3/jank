#!/usr/bin/env runhaskell

-- モナドの性質

import Data.Maybe
import Data.Monoid
import Data.Foldable
import Control.Monad
import Control.Applicative

-- モナド則
-- 1) (return x) >>= f == f x
-- 2) m >>= return     == m
-- 3) (m >>= f) >>= g  == (\x -> f x >>= g)

-- Maybeモナドの定義
-- data Maybe a = Nothing | Just a deriving (Eq, Ord)
-- instance Monad Maybe where
-- (Just x) >>= f = f x
-- Nothing  >>= _ = Nothing
-- return x       = Just x
-- tail _         = Nothing

-- リストモナドの定義
-- instance Monad [] where
-- xs >>= f = concatMap f xs = concat (map f xs) = join (fmap f xs)
--            fmap = map
--            join = concat
-- return x = [x]
-- return x :: [a]
-- f :: a -> [b]

-- Eitherモナドの定義
-- data Either a b = Left a | Right b

-- do式 : 次の2つの式は等価。
-- 1) main = do cs <- getContents
--              putStr cs
-- 2) main = getContents >>= putStr

-- do式 : 次の3つの式は等価。
-- 1) main = do putStrLn "Hello, World!"
--              putStrLn "Hello, again!"
-- 2) main = putStrLn "Hello, World!" >>= (\x -> putStrLn "Hello, again!")
-- 3) main = putStrLn "Hello, World!" >> putStrLn "Hello, again!"

-- MonadPlus規則
-- mzero >>= f == mzero
-- v >> mzero  == mzero

