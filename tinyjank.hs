#!/usr/bin/env runhaskell

-- 機能: 麻雀の役判定と点数計算
-- 作成: 2017-10-19  ken3@nurs.or.jp
-- 更新: 2017-10-25  ken3@nurs.or.jp

import Data.List

-- 表示用データ
image :: [String]
image =  [
    -- 端末が漢字表示に対応している場合はこちらの image を使用する。
    "■", "一", "二", "三", "四", "五", "六", "七", "八", "九",  -- 萬子
    "■", "①", "②", "③", "④", "⑤", "⑥", "⑦", "⑧", "⑨",  -- 筒子
    "■", "Ⅰ", "Ⅱ", "Ⅲ", "Ⅳ", "Ⅴ", "Ⅵ", "Ⅶ", "Ⅷ", "Ⅸ",  -- 索子
    "■", "東", "■", "南", "■", "西", "■", "北", "■", "■",  -- 風牌
    "■", "白", "■", "発", "■", "中"                         -- 三元牌
    {-
    -- 端末が漢字表示に対応していない場合はこちらの image を使用する。
    "00", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9",  -- 萬子
    "10", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9",  -- 筒子
    "20", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9",  -- 索子
    "30", "We", "32", "Ws", "34", "Ww", "36", "Wn", "38", "39",  -- 風牌
    "40", "Dw", "42", "Dg", "44", "Dr"                         -- 三元牌
    -}
    ]

-- 日本語文字列表示
show_as_utf8 :: Show a => a -> String
show_as_utf8 x = cons (show x)
  where cons :: String -> String
        cons [] = []
        cons m@(x:xs) | x == '\"' = str++(cons rest)
                      | x == '\'' = '\'':char:'\'':(cons rest')
                      | otherwise = x:cons xs
          where (str,rest):_   = reads m
                (char,rest'):_ = reads m

-- 牌種データ
wanzu  = [1..9]
pinzu  = [11..19]
souzu  = [21..29]
routou = [1,9,11,19,21,29]
kaze   = [31,33,35,37]
dragon = [41,43,45]
yaochu = [1,9,11,19,21,29,31,33,35,37,41,43,45]
greens = [22,23,24,26,28,43]
-- range  = [0..45]
range  = map fst $ zip [0..] image

-- 手牌の組み合わせ
data Hand  = Twins    [Int] |
             Triplets [Int] |
             Series   [Int] |
             Rest     [Int] |
             Kokushi  [Int] deriving (Show, Eq, Ord)

-- Handコンストラクタ
toTwins    :: [Int] -> Hand
toTriplets :: [Int] -> Hand
toSeries   :: [Int] -> Hand
toRest     :: [Int] -> Hand
toKokushi  :: [Int] -> Hand
toTwins    =  Twins
toTriplets =  Triplets
toSeries   =  Series
toRest     =  Rest
toKokushi  =  Kokushi

-- Handを[Int]に変換する
unbox :: Hand -> [Int]
unbox (Twins x)    = x ++ x
unbox (Triplets x) = x ++ x ++ x
unbox (Series x)   = foldr (\i -> (++) [i,i+1,i+2]) [] x
unbox (Rest x)     = x
unbox (Kokushi x)  = yaochu ++ x

-- ヒストグラムを返す
-- *Main> histogram [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
-- [0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,...,0,1,0,1,0,2]
histogram  :: [Int] -> [Int]
histogram  a = map (\n -> length $ filter (== n) a) range

-- Hand抽出関数
-- cons : タプルをHandに変換するコンバータ
-- cond : タプルの抽出条件
-- hs   : ヒストグラム
pick :: ((Int,Int) -> Hand) -> ((Int,Int) -> Bool) -> [Int] -> [Hand]
pick cons cond hs = map cons $ filter cond $ zip [0..] hs

-- 七対子判定
-- *Main> seven_pairs [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
-- [Twins [1,2,3,4,5,6,7]]
seven_pairs :: [Int] -> [Hand]
seven_pairs a | (length t) == 7 = t
              | otherwise = []
  where t = pick (toTwins . (:[]) . fst) ((== 2) . snd) (histogram a)

-- 国士無双判定
-- *Main> kokushi_muso [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
-- [Kokushi [45]]
kokushi_muso :: [Int] -> [Hand]
kokushi_muso a | (length p) == 14 && (length t) == 1 = t
               | otherwise = []
  where t = pick (toKokushi . (:[]) . fst) ((== 2) . snd) (histogram a)
        p = intersect a yaochu

-- 雀頭候補を返す
-- *Main> twins [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
-- [Twins [1],Twins [2],Twins [3],Twins [4],Twins [5],Twins [6]]
twins :: [Int] -> [Hand]
twins a = pick (toTwins . (:[]) . fst) ((>= 2) . snd) (histogram a)

-- 刻子候補を返す
-- *Main> triplets [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
-- [Triplets [5],Triplets [6]]
triplets :: [Int] -> [Hand]
triplets a = pick (toTriplets . (:[]) . fst) ((>= 3) . snd) (histogram a)

-- 順子候補を返す
-- *Main> series [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
-- [Series [1],Series [2],Series [3],Series [4]]
series :: [Int] -> [Hand]
series  a = pick (\(i,b) -> Series [i]) ((/= 0) . snd) (p $ histogram a)
  where p = (\h0@(_:h1@(_:h2)) -> zipWith3 (\x y z -> x*y*z) h0 h1 h2)

-- 差集合を返す(重複要素は残す)
-- *Main> subset [1,1,2,2,3,3,4,4,5,5,6,6,7,7] [1,2,3]
-- [1,2,3,4,4,5,5,6,6,7,7]
subset :: [Int] -> [Int] -> [Int]
subset a b = foldr delete a b

-- [[Hand]]を文字列化する
-- *Main > show_hands_array [[Triplets[22]],[Twins[19]]]
-- ["[[S2,S2,S2]]","[[P9,P9]]"]
show_hands_array :: [[Hand]] -> [String]
show_hands_array a = map show_hands a

-- [Hand]を文字列化する
-- *Main > show_hands [Kokushi [19]]
-- "[[M1,M9,P1,P9,P9,S1,S9,We,Ws,Ww,Wn,Dw,Dg,Dr]]"
show_hands :: [Hand] -> String
show_hands hs = "[" ++ (concatMap show_hand hs) ++ "]"

-- Handを文字列化する
-- *Main > show_hand $ Triplets[11]
-- "[P1,P1,P1]"
show_hand :: Hand -> String
show_hand (Twins x)    = concatMap (\n -> toImage [n,n]) x
show_hand (Triplets x) = concatMap (\n -> toImage [n,n,n]) x
show_hand (Series x)   = concatMap (\n -> toImage [n,n+1,n+2]) x
show_hand (Rest x)     = toImage $ sort x
show_hand (Kokushi x)  = toImage $ sort $ yaochu ++ x

-- 1メンツを文字列化する
toImage :: [Int] -> String
toImage x = show_as_utf8 $ map ((!!) image) x

-- アガリが成立する組み合せの集合を返す
-- *Main> p $ solve [1,9,11,19,21,29,31,33,35,37,41,43,45,19]
--     [Kokushi [19]]
-- *Main> p $ solve [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
--     [Twins [1,2,3,4,5,6,7]]
--     [Twins [1],Series [2,2,5,5]]
--     [Twins [4],Series [1,1,5,5]]
--     [Twins [7],Series [1,1,4,4]]
-- *Main> p $ solve [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
--     [Twins [1],Triplets [5,6],Series [2,2]]
--     [Twins [4],Triplets [5,6],Series [1,1]]
solve :: [Int] -> [[Hand]]
solve a = filter (not . null) $ r1:r2:r3
  -- 手牌から雀頭を括りだす
  -- a    = [1,1,9,9,19,20,21,31,43,44]
  -- ts   = [[1,1],[9,9]]
  -- body = [[Twins[1],Rest[9,9,19,20,21,31,43,44]],
  --         [Twins[9],Rest[1,1,19,20,21,31,43,44]]]
  where r1 = seven_pairs  a  -- 七対子判定
        r2 = kokushi_muso a  -- 国士無双判定
        r3 = solve' body     -- 1雀頭+N面子判定
        body = map (split . head . unbox) $ twins a
        split x = (Twins [x]):[Rest $ subset a [x,x]]

-- 1雀頭+N面子を確定する
-- Rest要素が無くなれば再帰呼び出しを終了する(more == 0)
-- *Main > solve' [[Twins[1],Rest[14,14,15,15,16,16,18,18,18]]]
-- [[Twins [1],Triplets [18],Series [14,14]]]
solve' :: [[Hand]] -> [[Hand]]
solve' hands | count == 0 = r0
             | otherwise  = solve' r0
  where r0 = nub $ breakdown hands -- nubを使って重複要素を削除する
        count = length $ filter has_rest r0
        has_rest []           = False
        has_rest ((Rest _):_) = True
        has_rest (_:xs)       = has_rest xs

-- Rest要素のメンツを1つ仮確定し、組み合わせを更新する
-- *Main > p $ breakdown [[Twins[1],Series[2],Rest[5,5,5,6,6,6,7,7,7]]]
--     [Twins [1],Triplets [5],Series [2],Rest [6,6,6,7,7,7]]
--     [Twins [1],Triplets [6],Series [2],Rest [5,5,5,7,7,7]]
--     [Twins [1],Triplets [7],Series [2],Rest [5,5,5,6,6,6]]
--     [Twins [1],Series [2,5],Rest [5,5,6,6,7,7]]
breakdown :: [[Hand]] -> [[Hand]]
breakdown xs = concatMap breakdown' xs
  where breakdown' :: [Hand] -> [[Hand]]
        breakdown' hands = map build candy
           where fixed   = find_fixed hands
                 candy   = find_mentsu_from $ unbox rest
                 rest    = head $ find_rest hands
                 rest'   = remove_from rest
                 build x = sorthands $ fixed ++ [x] ++ (rest' x)

-- Rest要素から仮確定したメンツを取り除く
-- *Main > remove_from (Rest[5,5,5,6,6,6,7,7,7]) (Series[5])
-- [Rest [5,5,6,6,7,7]]
-- *Main > remove_from (Rest[5,5,5,6,6,6,7,7,7]) (Triplets[5])
-- [Rest [6,6,6,7,7,7]]
remove_from :: Hand -> Hand -> [Hand]
remove_from hand candy | null array = []
                       | otherwise  = [Rest array]
  where array = subset (unbox hand) (unbox candy)

-- 未確定牌(Rest要素)の中にある刻子と順子を返す
-- *Main > find_mentsu_from [5,5,5,6,6,6,7,7,7]
-- [Triplets [5],Triplets [6],Triplets [7],Series [5]]
find_mentsu_from :: [Int] -> [Hand]
find_mentsu_from a = (triplets a) ++ (series  a)

-- 加算する
addhand :: Hand -> Hand -> Hand
addhand x y = Rest ((unbox x) ++ (unbox y))

-- 減算する
subhand :: Hand -> Hand -> Hand
subhand x y = Rest (subset (unbox x) (unbox y))

-- 手牌の並びを正規化する
-- *Main > sorthands $ twins [1,1,2,2,3,3,4,4,5,5,5,6,6,6]
-- [Twins [1,2,3,4,5,6]]
-- *Main > sorthands [Twins[1],Series[2],Rest[5,7],Rest[6,6,6],Twins[7,5]]
-- [Twins [1,5,7],Series [2],Rest [5,6,6,6,7]]
sorthands :: [Hand] -> [Hand]
sorthands x = (find_fixed x) ++ (find_rest x)

-- 手牌の中から確定牌(Rest要素以外)を探して返す
-- *Main> find_fixed [Twins[1],Series[2],Rest[5,5,5,6,6,6,7,7,7]]
-- [Twins [1],Series [2]]
find_fixed :: [Hand] -> [Hand]
find_fixed x = kokushi ++ twins ++ triplets ++ series
  where kokushi  = find_kokushi  x
        twins    = find_twins    x
        triplets = find_triplets x
        series   = find_series   x

-- 手牌の中から未確定牌(Rest要素)を探し、マージした結果を返す
-- *Main> find_rest [Twins[1],Series[2],Rest[5,5,5,6,6,6,7,7,7]]
-- [Rest [5,5,5,6,6,6,7,7,7]]
find_rest :: [Hand] -> [Hand]
find_rest hands | null r    = []
                | otherwise = [Rest $ sort r]
  where r = find_rest' [] hands
        find_rest' :: [Int] -> [Hand] -> [Int]
        find_rest' r []              = r
        find_rest' r (x@(Rest _):xs) = find_rest' (r ++ (unbox x)) xs
        find_rest' r (_:xs)          = find_rest' r xs

-- 手牌の中からTwins要素を探し、マージした結果を返す
-- *Main> find_twins [Twins[1],Twins[11,15],Twins[21],Series[2],Rest[5,7,9]]
-- [Twins [1,11,15,21]]
find_twins :: [Hand] -> [Hand]
find_twins hands | null r    = []
                 | otherwise = [Twins $ sort r]
  where r = find_twins' [] hands
        find_twins' :: [Int] -> [Hand] -> [Int]
        find_twins' r []               = r
        find_twins' r (x@(Twins m):xs) = find_twins' (r ++ m) xs
        find_twins' r (_:xs)           = find_twins' r xs

-- 手牌の中からTriplets要素を探し、マージした結果を返す
-- *Main> find_triplets [Twins[1],Triplets[11],Triplets[21],Series[2],Rest[5,7,9]]
-- [Triplets [11,21]]
find_triplets :: [Hand] -> [Hand]
find_triplets hands | null r    = []
                    | otherwise = [Triplets $ sort r]
  where r = find_triplets' [] hands
        find_triplets' :: [Int] -> [Hand] -> [Int]
        find_triplets' r []                  = r
        find_triplets' r (x@(Triplets m):xs) = find_triplets' (r ++ m) xs
        find_triplets' r (_:xs)              = find_triplets' r xs

-- 手牌の中からSeries要素を探し、マージした結果を返す
-- *Main> find_series [Twins[1],Triplets[11],Series[21],Series[2],Rest[5,7,9]]
-- [Series [2,21]]
find_series :: [Hand] -> [Hand]
find_series hands | null r    = []
                  | otherwise = [Series $ sort r]
  where r = find_series' [] hands
        find_series' :: [Int] -> [Hand] -> [Int]
        find_series' r []                = r
        find_series' r (x@(Series m):xs) = find_series' (r ++ m) xs
        find_series' r (_:xs)            = find_series' r xs

-- 手牌の中からKokushi要素を探して返す
-- 手牌の中のKokushi要素は高々1つしか無いはず。
-- *Main> find_kokushi [Kokushi[1]]
-- [Kokushi [1]]
find_kokushi :: [Hand] -> [Hand]
find_kokushi hands | null r    = []
                   | otherwise = [Kokushi $ sort r]
  where r = find_kokushi' hands
        find_kokushi' :: [Hand] -> [Int]
        find_kokushi' []                 = []
        find_kokushi' (x@(Kokushi m):xs) = m
        find_kokushi' (_:xs)             = find_kokushi' xs

-- 実行結果を出力するための共通サービス関数
generic_p :: ([Hand] -> String) -> [[Hand]] -> IO ()
generic_p f hands = mapM_ (\x -> putStrLn $ "    " ++ (f x)) hands

-- ASCIIリスト形式で出力する
p :: [[Hand]] -> IO ()
p = generic_p show

-- 麻雀牌形式で出力する
pp :: [[Hand]] -> IO ()
pp = generic_p (concatMap show_hand)

-- メイン関数
main :: IO ()
main = do putStrLn $ show m1
          pp $ solve m1
          putStrLn $ show m2
          pp $ solve m2
          putStrLn $ show m3
          pp $ solve m3
  where m1 = [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
        m2 = [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
        m3 = [1,1,2,2,2,3,3,3,4,4,4,26,27,28]

-- テスト
-- # 国士無双
-- *Main> p $ solve [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
-- [Kokushi [45]]
-- *Main> pp $ solve [1,9,11,19,21,29,31,33,35,37,41,43,45,45]
--     [M1,M9,P1,P9,S1,S9,We,Ws,Ww,Wn,Dw,Dg,Dr,Dr]
-- # 七対子
-- *Main> p $ solve [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
--     [Twins [1,2,3,4,5,6,7]]
--     [Twins [1],Series [2,2,5,5]]
--     [Twins [4],Series [1,1,5,5]]
--     [Twins [7],Series [1,1,4,4]]
-- *Main> pp $ solve [1,1,2,2,3,3,4,4,5,5,6,6,7,7]
--     [M1,M1][M2,M2][M3,M3][M4,M4][M5,M5][M6,M6][M7,M7]
--     [M1,M1][M2,M3,M4][M2,M3,M4][M5,M6,M7][M5,M6,M7]
--     [M4,M4][M1,M2,M3][M1,M2,M3][M5,M6,M7][M5,M6,M7]
--     [M7,M7][M1,M2,M3][M1,M2,M3][M4,M5,M6][M4,M5,M6]
-- # 三連刻
-- *Main> p $ solve [1,1,2,2,2,3,3,3,4,4,4,26,27,28]
--     [Twins [1],Triplets [2,3,4],Series [26]]
--     [Twins [1],Series [2,2,2,26]]
--     [Twins [4],Series [1,1,2,26]]
-- *Main> pp $ solve [1,1,2,2,2,3,3,3,4,4,4,26,27,28]
--     [M1,M1][M2,M2,M2][M3,M3,M3][M4,M4,M4][S6,S7,S8]
--     [M1,M1][M2,M3,M4][M2,M3,M4][M2,M3,M4][S6,S7,S8]
--     [M4,M4][M1,M2,M3][M1,M2,M3][M2,M3,M4][S6,S7,S8]

