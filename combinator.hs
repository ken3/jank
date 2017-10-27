#!/usr/bin/env runhaskell

-- Yコンビネータ
-- 不動点演算子 f(x) = x
-- Control.Monad.Fix モジュールの fix として標準的に含まれている。
-- import Control.Monad.Fix (fix)
y :: (t -> t) -> t
y x = x (y x)

-- 1+2+...+100を計算する
main :: IO ()
main = do
    print $ y (\f (x:xs) -> if (null xs) then x else x + f xs) [1..100]
    -- = 1 + f [2..99]
    -- = 1 + 2 + f [3..99]
    -- ...
    -- = 1 + 2 + ... + 98 + f [99]
    -- = 1 + 2 + ... + 98 + 99
    -- = 5050

