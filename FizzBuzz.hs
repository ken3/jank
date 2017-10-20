#!/usr/bin/env runghc

-- FizzBuzz
-- 作成: 2017-10-17 ken3@nurs.or.jp
-- 更新: 2017-10-19 ken3@nurs.or.jp
-- 参考: プログラムモグモグ 2015-12-27 Haskellで書かれたおもしろいFizzBuzz
--       ― Haskellで 読めないコードに遭遇した時に解読する方法を徹底解説！
--       http://itchyny.hatenablog.com/entry/2015/12/27/150000

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Applicative ((<*>))
import Control.Monad (MonadPlus)
import Control.Monad (guard)

-- stepの倍数に対して Just msg, それ以外には Nothing を返す基本関数
responder :: Integer -> String -> Integer -> Maybe String
responder step msg n = fmap (const msg) seed
          where seed = guard (n `mod` step == 0)

-- 3の倍数に対して Just "Fizz" を返す関数
fizz :: Integer -> Maybe String
fizz = responder 3 "Fizz"

-- 5の倍数に対して Just "Buzz" を返す関数
buzz :: Integer -> Maybe String
buzz = responder 5 "Buzz"

-- fizzとbuzzの合成関数
-- #  3の倍数に対して Just "Fizz"
-- #  5の倍数に対して Just "Buzz"
-- # 15の倍数に対して Just "FizzBuzz"
-- # それ以外に対しては Nothing を返す。
fizzbuzz :: Integer -> Maybe String
fizzbuzz = fizz <> buzz

-- FizzBuzzの挙動を実現する目的関数
-- #  3の倍数に対して "Fizz"
-- #  5の倍数に対して "Buzz"
-- # 15の倍数に対して "FizzBuzz"
-- # それ以外に対しては引数で与えた値をそのまま返す。
fizzbuzz' :: Integer -> String
fizzbuzz' n = fromMaybe (show n) (fizzbuzz n)

-- final answer
main :: IO ()
main = mapM_ putStrLn (map fizzbuzz' [1..30])

