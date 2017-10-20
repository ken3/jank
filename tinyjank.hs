#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

-- 機能: 麻雀の役判定と点数計算
-- 作成: 2017-10-19  ken3@nurs.or.jp
-- 更新: 2017-10-20  ken3@nurs.or.jp

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Data.List
import Data.Maybe
import Data.Monoid ((<>))
import Control.Applicative ((<*>))
import Control.Monad (MonadPlus)
import Control.Monad (guard)

-- 表示用データ
image :: [String]
image =  ["■", "一", "二", "三", "四", "五", "六", "七", "八", "九",  -- 萬子
          "■", "①", "②", "③", "④", "⑤", "⑥", "⑦", "⑧", "⑨",  -- 筒子
          "■", "Ⅰ", "Ⅱ", "Ⅲ", "Ⅳ", "Ⅴ", "Ⅵ", "Ⅶ", "Ⅷ", "Ⅸ",  -- 索子
          "■", "東", "■", "南", "■", "西", "■", "北", "■", "■",  -- 風牌
          "■", "白", "■", "発", "■", "中"]                        -- 三元牌
image' :: [String]
image' =  ["00", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9",  -- 萬子
           "10", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9",  -- 筒子
           "20", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9",  -- 索子
           "30", "We", "32", "Ws", "34", "Ww", "36", "Wn", "38", "39",  -- 風牌
           "40", "Dw", "42", "Dg", "44", "Dr"]                        -- 三元牌

-- 判定用データ
wanzu  = [1..9]
pinzu  = [11..19]
souzu  = [21..29]
routou = [1,9,11,19,21,29]
kaze   = [31,33,35,37]
dragon = [41,43,45]
yaochu = [1,9,11,19,21,29,31,33,35,37,41,43,45]
greens = [22,23,24,26,28,43]
range  = map fst $ zip [0..] image

-- 手牌の組み合わせ
data Hand  = Kokushi  Int   |
             Pairs    [Int] |
             Twins    Int   |
             Triplets Int   |
             Serials  Int   |
             Singles  [Int] |
             Rest     [Int] |
             Fixed    deriving (Show, Eq, Ord)

-- Handを[Int]に変換する
hand_to_intarray :: Hand -> [Int]
hand_to_intarray (Rest     x) = x
hand_to_intarray (Singles  x) = x
hand_to_intarray (Twins    x) = [x,x]
hand_to_intarray (Triplets x) = [x,x,x]
hand_to_intarray (Serials  x) = [x,x+1,x+2]
hand_to_intarray (Kokushi  x) = x:yaochu
hand_to_intarray (Pairs    x) = x++x

-- ヒストグラムを返す
-- *Main> map fst $ histogram [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
-- [0..45]
-- *Main> map snd $ histogram [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
-- [0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,0,0,0,1,0,1,0,2]
histogram :: [Int] -> [(Int,Int)]
histogram a = zip [0..] h
  where h = map (\n -> length $ filter (== n) a) range

-- 七対子判定
-- *Main> seven_pairs [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
-- [Pairs [1,2,3,4,5,6,7]]
seven_pairs :: [Int] -> [Hand]
seven_pairs a | cz == 7   = [Pairs ps]
              | otherwise = []
  where h  = histogram a
        z  = filter ((== 2) . snd) h
        cz = length z
        ps = map (\x -> fst x) z

-- 国士無双判定
-- *Main> kokushi_muso [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
-- [Kokushi 45]
kokushi_muso :: [Int] -> [Hand]
kokushi_muso a | cp == 14 && cz == 1 = [Kokushi t]
               | otherwise = []
  where h  = histogram a
        z  = filter ((== 2) . snd) h
        cz = length z
        t:ts = map fst z
        ps = intersect a yaochu
        cp = length ps

-- 雀頭候補を返す
-- *Main> twins [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
-- [Twins 1,Twins 2,Twins 3,Twins 4,Twins 5,Twins 6]
twins :: [Int] -> [Hand]
twins a = map (Twins . fst) z
  where h = histogram a
        z = filter ((>= 2) . snd) h

-- 刻子候補を返す
-- *Main> triplets [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
-- [Triplets 5,Triplets 6]
triplets :: [Int] -> [Hand]
triplets a = map (Triplets . fst) z
  where h = histogram a
        z = filter ((>= 3) . snd) h

-- 順子候補を返す
-- *Main> serials [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
-- [Serials 1,Serials 2,Serials 3,Serials 4]
serials :: [Int] -> [Hand]
serials a = map (\(i,b) -> Serials i) $ filter (\(i,b) -> b) r
  where h    = histogram a
        h0   = map snd h
        _:h1 = h0
        _:h2 = h1
        r = zip [0..] $ zipWith3 (\x y z -> x*y*z/=0) h0 h1 h2

-- 差集合を返す
-- 重複要素を削除しない。
-- *Main> subset [1,1,2,2,3,3,4,4,5,5,6,6,7,7] [1,2,3]
-- [1,2,3,4,4,5,5,6,6,7,7]
subset :: [Int] -> [Int] -> [Int]
subset a b = foldr delete a b

-- [[Hand]]を文字列化する
-- *Main>  show_hands_array [[Triplets 22],[Twins 19]]
-- ["[22,22,22]","[19,19]"]
show_hands_array :: [[Hand]] -> [String]
show_hands_array a = map show_hands a

-- [Hand]を文字列化する
-- *Main> show_hands [Kokushi 19]
-- "[1,9,11,19,19,21,29,31,33,35,37,41,43,45]"
show_hands :: [Hand] -> String
show_hands hs = concatMap show_hand hs

-- Handを文字列化する
-- *Main> show_hand (Triplets 11)
-- "[11,11,11]"
show_hand :: Hand -> String
show_hand (Rest     x) = show $ x
-- show_hand (Rest  x) = show $ map (\x -> image' !! x) x
show_hand (Singles  x) = show $ x
show_hand (Twins    x) = show $ [x,x]
show_hand (Triplets x) = show $ [x,x,x]
show_hand (Serials  x) = show $ [x,x+1,x+2]
show_hand (Kokushi  x) = show . sort $ x:yaochu
show_hand (Pairs    x) = show . sort $ x++x
show_hand otherwise       = "<unsure>"

-- アガリが成立する組み合せの集合を返す
-- *Main> p $ solve [1,9,11,19,21,29,31,33,35,37,41,43,45,19]
--     [Kokushi 19]
-- *Main> p $ solve [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
--     [Pairs [1,2,3,4,5,6,7]]
--     [Twins 1,Serials 2,Serials 2,Serials 5,Serials 5]
--     [Twins 4,Serials 1,Serials 1,Serials 5,Serials 5]
--     [Twins 7,Serials 1,Serials 1,Serials 4,Serials 4]
-- *Main> p $ solve [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
--     [Twins 1,Triplets 5,Triplets 6,Serials 2,Serials 2]
--     [Twins 4,Triplets 5,Triplets 6,Serials 1,Serials 1]
solve :: [Int] -> [[Hand]]
solve arr = filter (not . null) $ r3 ++ r2:r1:[]
  -- 手牌から雀頭を括りだす
  -- arr   = [1,1,9,9,19,20,21,31,43,44]
  -- ts    = [[1,1],[9,9]]
  -- hands = [[Twins 1,Rest [9,9,19,20,21,31,43,44]],
  --          [Twins 9,Rest [1,1,19,20,21,31,43,44]]]
  where r1  = seven_pairs  arr -- 七対子判定
        r2  = kokushi_muso arr -- 国士無双判定
        r3  = solve' hands  -- 1雀頭+N面子判定
        ts  = map hand_to_intarray $ twins arr
        hands = map (\x -> let i:is=x in (Twins i):[Rest $ subset arr x]) ts

-- 1雀頭+N面子を確定する
-- Rest要素が無くなれば再帰呼び出しを終了する(more == 0)
-- *Main> solve' [[Twins 1,Rest [14,14,15,15,16,16,18,18,18]]]
-- [[Twins 1,Triplets 18,Serials 14,Serials 14]]
solve' :: [[Hand]] -> [[Hand]]
solve' hands = if (more == 0) then r0 else solve' r0
  where r0 = nub $ breakdown hands -- nubを使って重複要素を削除する
        more = length $ filter findrest r0
        findrest hs = case hs of
            (Rest _):_ -> True
            _:xs       -> findrest xs
            []         -> False

-- Rest要素のメンツを1つ仮確定し、組み合わせを更新する
-- *Main> p $ breakdown [[Twins 1,Serials 2,Rest [5,5,5,6,6,6,7,7,7]]]
--     [Twins 1,Triplets 5,Serials 2,Rest [6,6,6,7,7,7]]
--     [Twins 1,Triplets 6,Serials 2,Rest [5,5,5,7,7,7]]
--     [Twins 1,Triplets 7,Serials 2,Rest [5,5,5,6,6,6]]
--     [Twins 1,Serials 2,Serials 5,Rest [5,5,6,6,7,7]]
breakdown :: [[Hand]] -> [[Hand]]
breakdown hands = concatMap breakdown' hands

-- Rest要素のメンツを1つ仮確定し、組み合わせを更新する
-- *Main> p $ breakdown' [Twins 1,Serials 2,Rest [5,5,5,6,6,6,7,7,7]]
--     [Twins 1,Triplets 5,Serials 2,Rest [6,6,6,7,7,7]]
--     [Twins 1,Triplets 6,Serials 2,Rest [5,5,5,7,7,7]]
--     [Twins 1,Triplets 7,Serials 2,Rest [5,5,5,6,6,6]]
--     [Twins 1,Serials 2,Serials 5,Rest [5,5,6,6,7,7]]
breakdown' :: [Hand] -> [[Hand]]
breakdown' hands = map (\x -> sort $ fixed ++ [x] ++ (newrest x)) candy
    where  rest  = search_rest  hands
           fixed = search_fixed hands
           candy = find_mentsu_from hands
           newrest x = remove_from_hand (head rest) x

-- Rest要素から仮確定したメンツを取り除く
-- *Main> remove_from_hand (Rest [5,5,5,6,6,6,7,7,7]) (Serials 5)
-- [Rest [5,5,6,6,7,7]]
-- *Main> remove_from_hand (Rest [5,5,5,6,6,6,7,7,7]) (Triplets 5)
-- [Rest [6,6,6,7,7,7]]
remove_from_hand :: Hand -> Hand -> [Hand]
remove_from_hand rest candy = if (null array) then [] else [Rest array]
    where rest_array  = hand_to_intarray rest
          candy_array = hand_to_intarray candy
          array = subset rest_array candy_array

-- 未確定牌(Rest要素)の中にある刻子と順子を返す
-- *Main> find_mentsu_from [Twins 1,Serials 2,Rest [5,5,5,6,6,6,7,7,7]]
-- [Triplets 5,Triplets 6,Triplets 7,Serials 5]
find_mentsu_from :: [Hand] -> [Hand]
find_mentsu_from hands = ts ++ ss
    where rest = hand_to_intarray $ head $ search_rest hands
          ts = triplets rest
          ss = serials  rest

-- 手牌の中から未確定牌(Rest要素)を探して返す
-- *Main> search_rest [Twins 1,Serials 2,Rest [5,5,5,6,6,6,7,7,7]]
-- [Rest [5,5,5,6,6,6,7,7,7]]
search_rest :: [Hand] -> [Hand]
search_rest hands = case hands of
   (Rest []):xs -> search_rest xs
   x@(Rest _):xs -> [x]
   _:xs -> search_rest xs
   []   -> []

-- 手牌の中から確定牌(Rest要素以外)を探して返す
-- *Main> search_fixed [Twins 1,Serials 2,Rest [5,5,5,6,6,6,7,7,7]]
-- [Twins 1,Serials 2]
search_fixed :: [Hand] -> [Hand]
search_fixed hands = case hands of
   (Rest _):xs -> search_fixed xs
   x:xs -> x:(search_fixed xs)
   []   -> []

-- 実行結果を出力するためのサービス関数
p hands = mapM_ (\x -> putStrLn $ "    "++(show x)) hands

-- テスト
-- # 国士無双
-- *Main> p $ solve [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
-- [Kokushi 45]
-- 
-- *Main> p $ solve [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
-- [Pairs [1,2,3,4,5,6,7]]
-- [Twins 1,Serials 2,Serials 2,Serials 5,Serials 5]
-- [Twins 4,Serials 1,Serials 1,Serials 5,Serials 5]
-- [Twins 7,Serials 1,Serials 1,Serials 4,Serials 4]
--
-- *Main> p $ solve [1,1,2,2,2,3,3,3,4,4,4,26,27,28]
-- [Twins 1,Triplets 2,Triplets 3,Triplets 4,Serials 26]
-- [Twins 1,Serials 2,Serials 2,Serials 2,Serials 26]
-- [Twins 4,Serials 1,Serials 1,Serials 2,Serials 26]

