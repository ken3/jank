#!/usr/bin/env runhaskell

-- Yコンビネータ
-- 不動点演算子 f(x) = x
-- Control.Monad.Fix モジュールの fix として標準的に含まれている。
-- import Control.Monad.Fix (fix)
y :: (t -> t) -> t
y x = x (y x)

-- 1+2+...+100を計算する
sum100 :: IO ()
sum100 = do
    print $ y (\f (x:xs) -> if (null xs) then x else x + f xs) [1..100]
    -- = 1 + f [2..99]
    -- = 1 + 2 + f [3..99]
    -- ...
    -- = 1 + 2 + ... + 98 + f [99]
    -- = 1 + 2 + ... + 98 + 99
    -- = 5050

-- チャーチ数
-- Church Encoding in Haskell
-- http://techtipshoge.blogspot.jp/2011/06/church-encoding-in-haskell.html

-- integer -> church
church n = \f -> \x -> iterate f x !! n

-- church -> integer
unChurch n = n succ 0

-- add
add = \m -> \n -> \f -> \x -> m f (n f x)

-- one
one = \f -> \x -> f x

-- n++
inc = \n -> \f -> \x -> f (n f x)

-- m*n
mult = \m -> \n -> \f -> \x -> m (n f) x

-- m^n
expt = \m -> \n -> n (mult m) one

main = do let a = church 2
              b = church 10
          print $ unChurch $ add a b   -- =>   12
          print $ unChurch $ inc a     -- =>    3
          print $ unChurch $ mult a b  -- =>   20
          print $ unChurch $ expt a b  -- => 1024

